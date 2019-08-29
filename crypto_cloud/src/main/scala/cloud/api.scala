package cloud

import boss._
import boss.jsany._
import cloud.Tools._
import models.{AbstractKeyJS, PrivateKey, PrivateKeyExported, SymmetricKey}
import org.scalajs.dom
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._
import tools.universa.{UniversaTools/*, logger*/}

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.JSON
import scala.scalajs.js.URIUtils._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray._
import scala.util.{Failure, Success}

@JSExportTopLevel("CryptoCloud.ApiError")
class ApiError(
  override val message: String = "CryptoCloud: API error",
  override val details: HashMap[String, Any] = HashMap[String, Any]()
) extends EncryptionError(message, details)

@JSExportTopLevel("CryptoCloud.NotFound")
class NotFound(
  override val message: String = "CryptoCloud: item not found",
  override val details: HashMap[String, Any] = HashMap[String, Any]()
) extends ApiError(message, details)

@JSExportTopLevel("CryptoCloud.WriteConflict")
class WriteConflict(
  override val message: String = "CryptoCloud: write conflict",
  override val details: HashMap[String, Any] = HashMap[String, Any]()
) extends ApiError(message, details)

object ApiError {
  def apply(message: String, details: HashMap[String, Any]): ApiError = {
    new ApiError(message, details)
  }

  def apply(details: HashMap[String, Any]): ApiError = {
    new ApiError("CryptoCloud: API error", details)
  }
}

object NotFound {
  def apply(message: String, details: HashMap[String, Any]): NotFound = {
    new NotFound(message, details)
  }

  def apply(details: HashMap[String, Any]): NotFound = {
    new NotFound("CryptoCloud: item not found", details)
  }
}

object WriteConflict {
  def apply(message: String, details: HashMap[String, Any]): WriteConflict = {
    new WriteConflict(message, details)
  }

  def apply(details: HashMap[String, Any]): WriteConflict = {
    new WriteConflict("CryptoCloud: write conflict", details)
  }
}

/** Collection of endpoint methods/helpers */
object CryptoCloud {
  val logCalls = false

  val API_PROVIDER_ORIGIN = "https://chatle.mainnetwork.io"
  val __ALWAYS_USE_NETWORK = false

  def deserialize(serialized: js.Dictionary[Any]): Future[Api] = {
    val privateKey = new PrivateKey(decode64(serialized("private_key").asInstanceOf[String]))
    val sessionToken = serialized("session_token").asInstanceOf[String]
    val partyId = serialized("party_id").asInstanceOf[Int]

    val api = new Api(privateKey, sessionToken, partyId)

    for {
      _ <- api.loadRegistry
    } yield api
  }

  /** Converts nick to binary token
    *
    * @param nick - nick to convert
    */
  def nickToToken(nick: String): Seq[Byte] = {
    val normalizedNick = nick.toLowerCase.replaceAll("\\s{2,}", " ").trim()
    if (normalizedNick.length < 4) throw new ApiError("normalized nick is too short")

    syntex1(normalizedNick)
  }

  @JSExportTopLevel("CryptoCloud.nickToToken")
  def nickToTokenJS(nick: String): js.Array[Byte] = nickToToken(nick).toJSArray

  /** Unpacks BOSS encoded capsule
    *
    * @param packed - BOSS encoded capsule
    */
  def unpackCapsule(packed: Seq[Byte]): Capsule = new Capsule(packed)

  /** Connects to cloud with nick and password
    *
    * @param appToken - secret app token to identify client
    * @param nick     - one of registered nicks
    * @param password - registry password
    */
  def connectWithPassword(
    appToken: String,
    nick: String,
    password: String
  ): Future[Api] = {

    def getRegistry =
      call(
        "registry_get",
        HashMap(
          "data" -> HashMap(
            "appToken" -> appToken,
            "nick_token" -> encode64(nickToToken(nick))
          )
        )
      ).asInstanceOf[Future[js.Dynamic]]

    def getPackedKey(registryResponse: js.Dynamic): Future[Seq[Byte]] = {
      Future {
        val raw = registryResponse.registry.asInstanceOf[String]
        if (raw == "") throw new AuthenticationError("unknown nick")

        val capsule = unpackCapsule(decode64(raw))
        if (capsule.publicData("tag") != "registry")
          throw new AuthenticationError("bad registry object")

        val d = capsule.decrypt(password)
        if (!d)
          throw AuthenticationError("access forbidden", HashMap(("code", "access_forbidden")))

        val registryData = capsule.privateData.get
        val mainKey = registryData("main_key").asInstanceOf[mutable.HashMap[String, Any]]

        val packedKey = readBytesSafe(mainKey("data"))
        val k = new PrivateKey(packedKey)
        val d2 = capsule.decrypt(k)
        if (!d2) throw new ApiError("can't unpack registry with its main key")

        packedKey
      }
    }

    for {
      registry <- getRegistry
      packedKey <- getPackedKey(registry)
      api <- connect(appToken, packedKey.toJSArray)
    } yield api
  }

  /** Connects to cloud with private key
    *
    * @param appToken         - secret app token to identify client
    * @param packedPrivateKey - BOSS encoded PrivateKey
    */
  def connect(
    appToken: String,
    packedPrivateKey: js.Array[Byte]
  ): Future[Api] = {
    val client_nonce = randomBytes(16)
    val privateKey = new models.PrivateKey(packedPrivateKey)

    def getNonce =
      call(
        "request_auth",
        HashMap("data" -> HashMap("appToken" -> appToken))
      ).asInstanceOf[Future[js.Dynamic]]

    def auth(response: js.Dynamic): Future[js.Dynamic] = {
      val id = response.id.asInstanceOf[Int]
      val nonce = response.nonce.asInstanceOf[String]
      val server_nonce = decode64(nonce)

      val payload = Boss.dump(HashMap(
        ("id", id),
        ("server_nonce", server_nonce),
        ("client_nonce", client_nonce)
      ))

      val signature = privateKey.sign(payload)

      call("auth", HashMap("data" -> HashMap(
        "id" -> id,
        "appToken" -> appToken,
        "payload" -> encode64(payload),
        "signature" -> encode64(signature),
        "key" -> encode64(privateKey.publicKey.pack())
      ))).asInstanceOf[Future[js.Dynamic]]
    }

    def getApi(data: js.Dynamic): Api = {
      if (data.clientNonce.asInstanceOf[String] != encode64(client_nonce))
        throw AuthenticationError(
          s"authentication failure (nonce failed)",
          HashMap(("code", "auth_failed"))
        )

      val sessionToken = data.sessionToken.asInstanceOf[String]
      val partyId = data.partyId.asInstanceOf[Int]

      new Api(privateKey, sessionToken, partyId)
    }

    for {
      nonceResponse <- getNonce
      authResponse <- auth(nonceResponse)
      api = getApi(authResponse)
      _ <- api.loadRegistry
    } yield api
  }

  /** Calls cloud API with given method and params
    *
    * @param methodName - name of the method
    * @param options    - hashmap of call options
    */
  def call(
    methodName: String,
    options: HashMap[String, Any] = HashMap[String, Any]()
  ): Future[Any] = {
    val p = Promise[Any]()
    val xhr = new dom.XMLHttpRequest()
    xhr.onload = { e: dom.Event =>
      val status = xhr.status
      if (status >= 200 && status < 300) {
        val data = JSON.parse(xhr.response.asInstanceOf[String]).asInstanceOf[js.Dynamic]
        val response = data.response.asInstanceOf[js.Dynamic]
        if (data.result.asInstanceOf[String] == "ok") {
          val encoded = response.encoded

          if (!js.isUndefined(encoded)) {
            val encodedMessage = encoded.asInstanceOf[String]
            try {
              p.success(Boss.load(decode64(encodedMessage)))
            } catch {
              case e: Throwable =>
                p.failure(e)
            }
          } else p.success(response)
        } else {

          p.failure(ApiError(HashMap(
            ("status", status),
            ("text", response.error),
            ("code", response.errorCode),
            ("errorClass", response.errorClass)
          )))
        }
      } else {
        p.failure(ApiError(HashMap(
          ("status", status),
          ("text", xhr.statusText)
        )))
      }
    }
    xhr.onerror = { e: dom.Event =>
      p.failure(ApiError(HashMap(
        ("status", xhr.status),
        ("text", xhr.statusText)
      )))
    }

    var url = s"/api/$methodName"
    url = s"$API_PROVIDER_ORIGIN$url"

    xhr.open("POST", url)
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")
    val headers = options.get("headers")

    headers match {
      case Some(value) => {
        for ((k, v) <- value.asInstanceOf[HashMap[String, String]]) {
          xhr.setRequestHeader(k, v)
        }
      }
      case None =>
    }

    var data = null
    val optData = options.get("data")

    optData match {
      case Some(value) =>
        val data = ListBuffer[String]()

        value.asInstanceOf[HashMap[String, Any]] foreach (x => {
          val dkey = encodeURIComponent(x._1.asInstanceOf[String])
          val dvalue = x._2 match {
            case s: String => encodeURIComponent(s)
            case n: Int => n
          }

          data += s"$dkey=$dvalue"
        })

        val args = data.mkString("&")

        xhr.send(args)
      case None => xhr.send()
    }

    p.future
  }

  @JSExportTopLevel("CryptoCloud.connect")
  def connectJS(appToken: String, param: js.Object): js.Promise[Api] = {
    if (!param.isInstanceOf[PrivateKeyExported]) {
      //it's UInt8Array
      connect(appToken, param.asInstanceOf[Int8Array].toJSArray).toJSPromise
    } else {
      connect(appToken, param.asInstanceOf[PrivateKeyExported].privateKey.pack.toJSArray).toJSPromise
    }
  }

  @JSExportTopLevel("CryptoCloud.connectWithPassword")
  def connectWithPasswordJS(
    appToken: String,
    nick: String,
    password: String
  ): js.Promise[Api] =
    CryptoCloud.connectWithPassword(appToken, nick, password).toJSPromise

  def execute(
    command: String,
    sessionToken: String,
    params: HashMap[String, Any] = HashMap[String, Any](),
    attempt: Int = 1
  ): Future[HashMap[String, Any]] = {
    if (attempt > 5)
      return Future.failed(new ApiError("Too many failed attempts", params))

    val data = HashMap(
      ("sessionToken", sessionToken),
      ("params", encode64(Boss.dump(params)))
    )

    def tryAgain(e: Throwable): Future[HashMap[String, Any]] = {
      for {
        wakeUp <- sleep(50)
        response <- execute(command, sessionToken, params, attempt + 1)
      } yield response
    }

    val mayFail = for {
      response <- CryptoCloud.call(command, HashMap("data" -> data))
    } yield {
      response.asInstanceOf[HashMap[String, Any]]
    }

    mayFail recoverWith { case e => tryAgain(e) }
  }

  def loadItemsForWorker(
    sessionToken: String,
    tagName: String,
    postMessageFunction: js.Function1[js.Any, Unit]
  ): Unit = {
    println("Run load items for worker")
    val itemsResult = Tools.importAllSerializedObjects(sessionToken, tagName)
    itemsResult.onComplete {
      case Success(value) =>
        val values: js.Array[String] = value.map(v => UniversaTools.encode64(v))
        val resultArray = js.Array("Items", values)
        postMessageFunction(resultArray)
      case Failure(exception) =>
        postMessageFunction(exception.getMessage) //TODO to unify with privateKey use smth like js.Dynamic(...)
    }
  }

  def loadItemsWorker(
    serializedApi: js.Dictionary[Any],
    options: js.Dictionary[Any],
    callback: js.Function1[js.Any, Unit]
  ): Unit = {
    deserialize(serializedApi) map {
      api => api.loadItemsEncrypted(options).onComplete {
        case Success(items) =>
          val values = items.map(v => UniversaTools.encode64(Boss.dump(v))).toJSArray
          callback(js.Array("Items", values))
        case Failure(exception) =>
          callback(exception.getMessage) //TODO to unify with privateKey use smth like js.Dynamic(...)
      }
    }
  }

  def loadItemsAndSerialize(
    sessionToken: String,
    options: mutable.HashMap[String, Any]
  ): Future[ListBuffer[(Option[Int], Seq[Byte])]] = {
    val afterSerial = options.getOrElse("afterSerial", null)
    val beforeSerial = options.getOrElse("beforeSerial", null)
    var latestSerial = options.getOrElse("latestSerial", null)
    val tags = options("tags").asInstanceOf[ListBuffer[String]]
    var limit = options.getOrElse("limit", 100)

    if (latestSerial != null && latestSerial.isInstanceOf[Int]) {
      limit = latestSerial
      latestSerial = true
    }

    val opts = mutable.HashMap(
      ("tags", tags),
      ("after_serial", afterSerial),
      ("before_serial", beforeSerial),
      ("latest_serial", latestSerial),
      ("limit", limit)
    )

    def itemsLoad = execute("items_load", sessionToken, opts)

    def makeItems(result: mutable.HashMap[String, Any]): ListBuffer[(Option[Int], Seq[Byte])] = {
      val mayBeItems = result.get("items").asInstanceOf[Option[ListBuffer[mutable.HashMap[String, Any]]]]

      mayBeItems match {
        case None => ListBuffer.empty
        case Some(items) =>
          if (items.isEmpty) ListBuffer.empty
          else {
            val serials = items.map(_.get("serial").asInstanceOf[Option[Int]])
            val encItems = items.map(map => Boss.dump(map))
            serials.zip(encItems)
          }
      }
    }

    for {
      result <- itemsLoad.recoverWith { case anyException =>
        itemsLoad
      }
      loadedItems = makeItems(result)
    } yield loadedItems
  }
}

/** The CryptoCloud main service point. The Java's Service analogue, somewhat more
  * advanced and convenient as is more user-centric than service-oriented.
  *
  * @constructor creates instance of API
  * @param privateKey   - instance of PrivateKey
  * @param sessionToken - session token received by cloud authentication
  * @param partyId      - party id received by cloud authentication
  */
@JSExportTopLevel("CryptoCloud.Api")
class Api(
  val privateKey: models.PrivateKey,
  val sessionToken: String,
  val partyId: Int
) {
  type AnyMap = HashMap[String, Any]

  private val itemCache = HashMap.empty[String, Any]
  private val parties = HashMap.empty[Int, Party]
  private val connectors = HashMap.empty[Int, Connector]

  @JSExport("sessionToken")
  def sessionTokenJS = sessionToken

  @JSExport("serialized")
  def serialized = HashMap(
    ("private_key", encode64(privateKey.pack)),
    ("session_token", sessionToken),
    ("party_id", partyId)
  ).toJSDictionary

  /** Represents current API user */
  @JSExport
  var me = new Party(
    this,
    new Connector(this, HashMap[String, Any]()),
    HashMap(("id", partyId))
  )

  var registry: Option[Registry] = None

  /** Sends API command in current session
    *
    * @param command - name of command
    * @param params  - command's params
    */
  def execute(
    command: String,
    params: HashMap[String, Any] = HashMap[String, Any](),
    attempt: Int = 1
  ): Future[HashMap[String, Any]] = {
    if (attempt > 5)
      return Future.failed(new ApiError("Too many failed attempts", params))

    val data = HashMap(
      ("sessionToken", sessionToken),
      ("params", encode64(Boss.dump(params)))
    )

    def tryAgain(e: Throwable): Future[HashMap[String, Any]] = {
      for {
        wakeUp <- sleep(50)
        response <- execute(command, params, attempt + 1)
      } yield response
    }

    val mayFail = for {
      response <- CryptoCloud.call(command, HashMap("data" -> data))
    } yield {
      response.asInstanceOf[HashMap[String, Any]]
    }

    mayFail recoverWith { case e => tryAgain(e) }
  }

  /** Simple command to check if session is alive */
  def ping: Future[HashMap[String, Any]] =
    execute("ping", HashMap("value" -> "foo"))

  /** Returns key ring of current session */
  def getKeyRing: KeyRing = {
    def simpleRing: KeyRing = new SimpleKeyRing(ListBuffer(privateKey))

    registry match {
      case Some(reg) => reg.storageKey match {
        case Some(key) => new SimpleKeyRing(ListBuffer(key, privateKey))
        case None => simpleRing
      }
      case None => simpleRing
    }
  }

  /** Returns cloud storage key used in encryption */
  def storageKey: Option[SymmetricKey] = registry.flatMap(_.storageKey)

  /** Tries to register nick with option
    *
    * @param nick       - nick to register
    * @param searchable - mark nick to make searchable by other users
    */
  def registerNick(nick: String, searchable: Boolean = true): Future[Boolean] = {
    var token = Seq[Byte]()

    try {
      token = CryptoCloud.nickToToken(nick)
    } catch {
      case e: Exception => return Future.failed(e)
    }

    val addToken = execute("token_add_nick", HashMap(
      ("nick_token", token),
      ("searchable", searchable)
    ))

    def updateRegistry(response: HashMap[String, Any]): Future[Boolean] = {
      val mayBeToken = response.get("token")

      if (mayBeToken.isEmpty) return Future.successful(false)

      val receivedToken = mayBeToken.get.asInstanceOf[Seq[Byte]]

      if (encode64(receivedToken) != encode64(token))
        return Future.failed(new ApiError("addToken internal error: tokens mismatch"))

      nicks(nick) = searchable

      registry.get.save map {
        _ => true
      }
    }

    for {
      response <- addToken
      isDone <- updateRegistry(response)
    } yield isDone
  }

  /** Unregisters nick
    *
    * @param nick - nick to unregister
    */
  def unregisterNick(nick: String): Future[Any] = {
    val token = CryptoCloud.nickToToken(nick)
    val removeToken = execute("token_remove", HashMap(("nick_token", token))) recover {
      case _: ApiError => false
      case e: Exception => println(e)
    }

    def updateRegistry(response: Any): Future[Any] = {
      if (nicks.contains(nick)) {
        nicks -= nick
        registry.get.save
      } else Future.successful(true)
    }

    for {
      response <- removeToken
      _ <- updateRegistry(response)
    } yield response
  }

  /** Load registry of current API user */
  def loadRegistry: Future[Any] = {
    registry = Some(new Registry(this))
    registry.get.load
  }

  /** Returns hash of registered nicks and their searchable options */
  def nicks: HashMap[String, Boolean] = registry.get.nicks

  private def reloadConnectors: Future[Boolean] = {
    execute("connectors_list") map {
      response =>
        val list = response("connectors").asInstanceOf[ListBuffer[HashMap[String, Any]]]
        list.map(updateConnector)
        true
    }
  }

  private def updateConnector(data: HashMap[String, Any]): Option[Party] = {
    val connectorId = data("id").asInstanceOf[Int]
    val self = this
    val existing = connectors.get(connectorId)

    val conn = existing match {
      case Some(c) =>
        c.update(data)
        c
      case None =>
        connectors(connectorId) = new Connector(self, data)
        connectors(connectorId)
    }

    partyForConnector(conn)
  }

  private def partyForConnector(connector: Connector): Option[Party] = {
    val self = this
    val pp = connector.parties
    if (pp.size < 2) return None

    var otherParty = pp.head
    if (otherParty("id").asInstanceOf[Int] == partyId) otherParty = pp(1)
    val otherPartyId = otherParty("id").asInstanceOf[Int]
    val party = parties.get(otherPartyId)

    party match {
      case None =>
        val p = new Party(self, connector, otherParty)
        parties(otherPartyId) = p
        Some(p)
      case Some(p) =>
        p.connector = connector
        p.partyData = otherParty
        Some(p)
    }
  }

  /** Gets connector by ID
    *
    * @param connectorId - connector id
    */
  def getConnector(connectorId: Int): Future[Connector] = {
    val existing = connectors.get(connectorId)

    if (existing.isDefined) Future.successful(existing.get)
    else {
      reloadConnectors map {
        _ =>
          val exist = connectors.get(connectorId)
          exist.getOrElse(throw new ApiError(s"Bad connector id: $connectorId"))
      }
    }
  }

  /** Returns party by nick or party's id if found
    *
    * @param options - hashmap contains nick or partyId
    */
  def getParty(options: HashMap[String, Any]): Future[Option[Party]] = {
    val mayBeNick = options.get("nick")
    val mayBeId = options.get("partyId")
    val self = this

    def getByNick(nick: String): Future[Option[Party]] = {
      val getter = execute("connectors_request",
        HashMap(("nick_token", CryptoCloud.nickToToken(nick)))
      )
      getter map {
        response =>
          val data = response.get("connector")
          data.flatMap(d => updateConnector(d.asInstanceOf[HashMap[String, Any]]))
      }
    }

    def getById(id: Int): Future[Option[Party]] = {
      if (id == partyId) Future.successful(Some(me))
      else {
        val p = parties.get(id)
        p match {
          case Some(party) => Future.successful(Some(party))
          case None =>
            reloadConnectors map { _ =>
              val mayBeParty = parties.get(id)
              mayBeParty.orElse {
                println(s"creating party without connector for $id, my is $partyId")
                parties(id) = new Party(self, new Connector(self, HashMap[String, Any]()), HashMap(("id", id)))
                Some(parties(id))
              }
            }
        }
      }
    }

    mayBeNick match {
      case Some(nick) => getByNick(nick.asInstanceOf[String])
      case None =>
        mayBeId match {
          case Some(id) => getById(id.asInstanceOf[Int])
          case None => Future.successful(None)
        }
    }
  }

  /** Returns item by id or tag
    *
    * @param constructor - new item constructor function
    * @param options     - hashmap contains item's id or tag
    */
  def getItem(
    constructor: Api => Item,
    options: HashMap[String, Any]
  ): Future[Item] = {
    val item = constructor(this)

    options.get("id") match {
      case Some(id) =>
        item.loadId(id.asInstanceOf[Int]) map { _ => item }
      case None => options.get("tag") match {
        case Some(tag) => item.loadTag(tag.asInstanceOf[String]) map { _ => item }
        case None => throw new ApiError("item tag or id must be provided")
      }
    }
  }

  /** Sets password for current API user's registry
    *
    * @param password - new registry password
    */
  def setPassword(password: String): Future[js.Dictionary[Any]] = {
    registry match {
      case Some(reg) => reg.setPassword(password).map(_.toJSDictionary)
      case None => throw new ApiError("Registry is not loaded")
    }
  }

  def loadItemsEncrypted(options: AnyMap): Future[ListBuffer[AnyMap]] = {
    val afterSerial = options.getOrElse("afterSerial", null)
    val beforeSerial = options.getOrElse("beforeSerial", null)
    var latestSerial = options.getOrElse("latestSerial", null)
    val tags = options("tags").asInstanceOf[ListBuffer[String]]
    var limit = options.getOrElse("limit", 100)

    if (latestSerial != null && latestSerial.isInstanceOf[Int]) {
      limit = latestSerial
      latestSerial = true
    }

    val opts = HashMap(
      ("tags", tags),
      ("after_serial", afterSerial),
      ("before_serial", beforeSerial),
      ("latest_serial", latestSerial),
      ("limit", limit)
    )

    def itemsLoad = execute("items_load", opts)

    for {
      response <- itemsLoad.recoverWith { case anyException => itemsLoad }
    } yield {
      response.get("items").asInstanceOf[Option[ListBuffer[AnyMap]]] match {
        case None => ListBuffer[AnyMap]()
        case Some(items) => {
          if (items.isEmpty) ListBuffer[AnyMap]()
          else items
        }
      }
    }
  }

  /** Load items by options
    *
    * @param options - hashmap of options.
    *                (Int) options.afterSerial   - load items after given serial number
    *                (Int) options.beforeSerial  - load items before given serial number
    *                (Int) options.latestSerial  - load items up to last serial
    *                (List[String]) options.tags - array of item tags to load
    *                (Int) options.limit         - load up to this number of items
    */
  def loadItems(options: HashMap[String, Any]): Future[ListBuffer[Item]] = {
    val afterSerial = options.getOrElse("afterSerial", null)
    val beforeSerial = options.getOrElse("beforeSerial", null)
    var latestSerial = options.getOrElse("latestSerial", null)
    val tags = options("tags").asInstanceOf[ListBuffer[String]]
    var limit = options.getOrElse("limit", 100)
    val emptyList = ListBuffer[Option[Item]]()

    if (latestSerial != null && latestSerial.isInstanceOf[Int]) {
      limit = latestSerial
      latestSerial = true
    }

    val opts = HashMap(
      ("tags", tags),
      ("after_serial", afterSerial),
      ("before_serial", beforeSerial),
      ("latest_serial", latestSerial),
      ("limit", limit)
    )

    def itemsLoad = execute("items_load", opts)

    def makeItems(result: HashMap[String, Any]): Future[ListBuffer[Option[Item]]] = {
      val mayBeItems = result.get("items").asInstanceOf[Option[ListBuffer[HashMap[String, Any]]]]

      mayBeItems match {
        case None => Future.successful(emptyList)
        case Some(items) =>
          if (items.isEmpty)
            Future.successful(emptyList)
          else {
            val loaders = items.map(makeItem(_).recover { case _: Throwable => None })
            Future.sequence(loaders)
          }
      }
    }

    for {
      result <- itemsLoad.recoverWith { case anyException =>
//        logger.error(s"Retrying because of: ${anyException.toString}")
        itemsLoad
      }
      loadedItems <- makeItems(result)
    } yield loadedItems.flatten
  }

  def makeItem(itemRecord: HashMap[String, Any]): Future[Option[Item]] = {
    // logger.enter(s"makeItem()")

    val self = this
    val cap = new Capsule(itemRecord("data").asInstanceOf[Seq[Byte]])
    val tag = cap.publicData("tag")
    val i = tag match {
      case "comm" => new Message(self)
      case _ => new Item(self)
    }

    i.initialize
    i.loadCapsule(cap)
    i.record = Some(itemRecord)

    i.setupLoadedData map {
      done =>
        i.privateData match {
          case None =>
            println(s"item decryption error, $i")
            None
          case Some(data) =>
            if (i.isInstanceOf[Message] && i.priv("type") == "profile")
              i.asInstanceOf[Message].from.get.updateProfile(i)

            Some(i)
        }
    }
  }

  /** Reloads all parties info */
  def reloadParties: Future[Boolean] = reloadConnectors

  @JSExport("reloadParties")
  def reloadPartiesJS(): js.Promise[Boolean] = reloadParties

  @JSExport("ping")
  def pingJS(): js.Promise[js.Dictionary[Any]] = ping

  @JSExport("registerNick")
  def registerNickJS(nick: String, searchable: Boolean = true): js.Promise[Boolean] =
    registerNick(nick, searchable)

  @JSExport("unregisterNick")
  def unregisterNickJS(nick: String): js.Promise[Any] =
    unregisterNick(nick)

  @JSExport("nicks")
  def nicksJS: js.Dictionary[Boolean] = nicks.toJSDictionary

  @JSExport("execute")
  def executeJS(
    command: String,
    params: js.Dictionary[Any] = js.Dictionary[Any]()
  ): js.Promise[js.Dictionary[Any]] = execute(command, params)

  @JSExport("getParty")
  def getPartyJS(options: js.Dictionary[Any]): js.Promise[Any] = {
    getParty(options).map(_.orNull).toJSPromise
  }

  @JSExport("setPassword")
  def setPasswordJS(password: String): js.Promise[Any] = setPassword(password)

  @JSExport("partyId")
  val partyIdJS = partyId

  @JSExport("registry")
  def registryJS: Registry = registry.get

  @JSExport("privateKey")
  def privateKeyJS: PrivateKeyExported = new PrivateKeyExported(privateKey)

  @JSExport("getItem")
  def getItemJS(
    constructor: js.Function1[Api, Item],
    options: js.Dictionary[Any]
  ): js.Promise[Item] = getItem(constructor, options).toJSPromise

  @JSExport("storageKey")
  def storageKeyJS: AbstractKeyJS = registry.get.storageKeyJS

  @JSExport("loadItems")
  def loadItemsJS(
    options: js.Dictionary[Any]
  ): js.Promise[js.Array[Item]] = {
    loadItems(options)
      .map(_.toJSArray)
      .toJSPromise
  }
}



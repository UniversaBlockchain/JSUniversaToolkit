package node

import models.{HashId, PrivateKey, PrivateKeyExported, PublicKey}
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

case class Node(url: String, key: PublicKey)

@JSExportTopLevel("Node")
class NodeApiExported(val nodeApi: NodeApi) extends js.Object {
  type jsDict = js.Dictionary[Any]
  type jsBytes = js.Array[Byte]

  def command(
                 name: String,
                 params: jsDict = js.Dictionary[Any]()
               ): js.Promise[jsDict] = nodeApi.command(name, params)

  def checkContract(id: jsBytes): js.Promise[jsDict] =
    nodeApi.checkContract(HashId.fromBytes(id))

  def checkParcel(id: jsBytes): js.Promise[jsDict] =
    nodeApi.checkParcel(HashId.fromBytes(id))

  def registerContract(packed: jsBytes): js.Promise[jsDict] =
    nodeApi.registerContract(packed)

  def registerParcel(packed: jsBytes): js.Promise[jsDict] =
    nodeApi.registerParcel(packed)
}

/** Represents node API
 *
 * @param privateKey - PrivateKey instance to authorize with
 * @param debugMode - if true, will call localhost
 */
class NodeApi(val privateKey: PrivateKey, debugMode: Boolean = false) {
  val connections = HashMap[Int, NodeConnection]()
  var nodes = ListBuffer[Node]()
  var ready = Promise[Boolean]()
  var version: Option[String] = None
  var startUrl: Option[String] = None
  var nodeKey: Option[PublicKey] = None

  type jsDict = js.Dictionary[Any]
  type jsBytes = js.Array[Byte]

  /** Will try to connect Node until success */
  def connect: Future[NodeApi] = {
    val self = this

    val startUrl1 =
      if (debugMode) "http://localhost:80"
      else "https://node-1-com.universa.io:443"

    def tryAgain(e: Throwable): Future[NodeApi] = {
      println(e)

      for {
        wakeUp <- sleep(1000)
        api <- connect
      } yield api
    }

    val mayFail = for {
      networkData <- NodeConnection.request("GET", s"$startUrl1/network")
    } yield {
      val givenVersion = networkData("version").asInstanceOf[String]
      version = Some(givenVersion)

      nodes = ListBuffer[Node]()

      for (n <- networkData("nodes").asInstanceOf[ListBuffer[HashMap[String, Any]]]) {
        nodes += Node(n("url").asInstanceOf[String], new PublicKey(n("key").asInstanceOf[Seq[Byte]]))
      }

      val nodesTotal = nodes.size

      println(s"Connecting to the Universa network v$givenVersion")
      println(s"Loaded network configuration, $nodesTotal nodes")

      ready.success(true)
      self
    }

    mayFail recoverWith { case e => tryAgain(e) }
  }

  def getNode(idx: Int): Future[NodeConnection] = {
    def connectToNode: Future[NodeConnection] = {
      val info = nodes(idx)
      val url = info.url.replace("http:", "https:").replace(":8080", ":443")
      val nodeConnection = Some(new NodeConnection(url, info.key, privateKey))

      nodeConnection.get.connect.map(conn => {
        connections(idx) = conn
        conn
      })
    }

    if (connections.contains(idx)) return Future.successful(connections(idx))
    else {
      for {
        isReady <- ready.future
        con <- connectToNode
      } yield con
    }
  }

  def connection: Future[NodeConnection] = {
    for {
      isReady <- ready.future
      con <- getNode(randomInRange(0, nodes.size - 1))
    } yield con
  }

  /** Sends command to Node
   *
   * @param name - command's name
   * @param params - command's options
   */
  def command(
    name: String,
    params: HashMap[String, Any] = HashMap[String, Any]()
  ): Future[HashMap[String, Any]] = {
    def tryAgain(e: Throwable): Future[HashMap[String, Any]] = {
      println(e)
      for {
        _ <- sleep(1000)
        _ <- connect
        api <- command(name, params)
      } yield api
    }

    for {
      con <- connection
      response <- con.command(name, params) recoverWith { case e => tryAgain(e) }
    } yield response
  }

  /** Register the contract with the Universa network
   *
   * @param packed - BOSS encoded contract
   */
  def registerContract(packed: Seq[Byte]): Future[HashMap[String, Any]] =
    command("approve", HashMap(("packedItem", packed)))

  /** Register the parcel with the Universa network
   *
   * @param packed - BOSS encoded parcel
   */
  def registerParcel(packed: Seq[Byte]): Future[HashMap[String, Any]] =
    command("approveParcel", HashMap(("packedItem", packed)))

  /** Checks contract state by id
   *
   * @param id - contract ID
   */
  def checkContract(id: HashId): Future[HashMap[String, Any]] =
    command("getState", HashMap(("itemId", id)))

  /** Checks parcel state by id
   *
   * @param id - parcel ID
   */
  def checkParcel(id: HashId): Future[HashMap[String, Any]] =
    command("getParcelProcessingState", HashMap(("parcelId", id)))

  implicit def bytesToHashId(id: Seq[Byte]): HashId = HashId.fromBytes(id)
}

/** Factory for [[node.NodeApiExported]] instances. */
object NodeApiExported {
  /** Creates instance by PrivateKey
   *
   * @param privateKey - PrivateKey to authorize with
   */
  def apply(privateKey: PrivateKey): Future[NodeApiExported] = {
    val api = new NodeApi(privateKey)
    api.connect
      .map(nodeapi => new NodeApiExported(nodeapi))
  }

  @JSExportStatic("connect")
  def createJS(privateKey: PrivateKeyExported): js.Promise[NodeApiExported] =
    apply(privateKey.privateKey).toJSPromise
}

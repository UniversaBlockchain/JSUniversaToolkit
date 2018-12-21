package node

import boss._
import boss.jsany._
import models.SymmetricKey
import models.{HashId, PrivateKey, PublicKey}
import org.scalajs.dom
import tools.universa.UniversaTools._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.URIUtils._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

@JSExportTopLevel("Node.ApiError")
class ApiError(
  val message: String = "Node API error",
  val details: HashMap[String, Any] = HashMap[String, Any]()
) extends Exception(message) {
  @JSExport("message")
  val messageJS = message

  @JSExport("details")
  val detailsJS = details.toJSDictionary

  override def toString: String = s"EncryptionError: $message"
}

object ApiError {
  def apply(message: String, details: HashMap[String, Any]): ApiError = {
    new ApiError(message, details)
  }

  def apply(details: HashMap[String, Any]): ApiError = {
    new ApiError("Node API error", details)
  }
}

/** Represents node connection container
 *
 * @param url - url string to connect node
 * @param nodeKey - public key of node
 * @param clientKey - private key of current API user
 */
class NodeConnection(val url: String, nodeKey: PublicKey, clientKey: PrivateKey) {
  var sessionId: Option[String] = None
  var sessionKey: Option[SymmetricKey] = None
  var connectMessage: Option[String] = None

  /** Connects to Node */
  def connect: Future[NodeConnection] = {
    val self = this
    val safeURL = url.replace("http", "https").replace(":80", ":443")
    val clientNonce = randomBytes(47)

    println(s"setting up protected connection to $url")

    val connectReq = NodeConnection.bossRequest(s"$url/connect", HashMap(
      ("client_key", clientKey.publicKey.pack),
      ("jsapi", true)
    ))

    def getToken(connectData: HashMap[String, Any]): Future[HashMap[String, Any]] = {
      sessionId = Some(connectData("session_id").asInstanceOf[String])

      val data = Boss.dump(HashMap(
        ("client_nonce", clientNonce),
        ("server_nonce", connectData("server_nonce").asInstanceOf[Seq[Byte]])
      ))

      NodeConnection.bossRequest(s"$url/get_token", HashMap(
        ("signature", clientKey.sign(data)),
        ("session_id", sessionId.get),
        ("data", data)
      ))
    }

    def testConnection(token: mutable.HashMap[String, Any]): Future[NodeConnection] = {
      val data = token("data").asInstanceOf[Seq[Byte]]

      if (!nodeKey.verify(data, token("signature").asInstanceOf[Seq[Byte]]))
        return Future.failed(throw new ApiError("bad node signature"))

      val params = Boss.load(data).asInstanceOf[mutable.HashMap[String, Any]]

      if (encode64(clientNonce) != encode64(params("client_nonce").asInstanceOf[Seq[Byte]]))
        return Future.failed(throw new ApiError("nonce mismatch, authentication failed"))

      val encryptedToken = Boss.load(clientKey.decrypt(
        params("encrypted_token").asInstanceOf[Seq[Byte]]
      )).asInstanceOf[mutable.HashMap[String, Any]]

      val key = encryptedToken("sk").asInstanceOf[Seq[Byte]]

      sessionKey = Some(new SymmetricKey(key))

      command("hello") map {
        response =>
          val message = response("message").asInstanceOf[String]
          connectMessage = Some(message)

          val status = response("status").asInstanceOf[String]
          if (status != "OK")
            throw new ApiError(s"wrong status $status, authentication failed")

          println(s"connected, system says: $message")

          self
      }
    }

    for {
      connectData <- connectReq
      token <- getToken(connectData)
      connection <- testConnection(token)
    } yield connection
  }

  /** Executes command on Node
   *
   * @param name - command's name
   * @param params - command's options
   */
  def command(
    name: String,
    params: HashMap[String, Any] = HashMap[String, Any]()
  ): Future[HashMap[String, Any]] = {
    if (sessionKey.isEmpty) throw new ApiError("not in session")

    val sk = sessionKey.get

    val callData = Boss.dump(HashMap(
      ("command", name),
      ("params", params)
    ))

    NodeConnection.bossRequest(s"$url/command", HashMap(
      ("command", "command"),
      ("params", sk.encrypt(callData)),
      ("session_id", sessionId.get)
    )) map { response =>
        val encryptedResult = response("result").asInstanceOf[Seq[Byte]]
        val decrypted = sk.decrypt(encryptedResult)
        val result = Boss.load(decrypted).asInstanceOf[HashMap[String, Any]]

        result.get("error") match {
          case None => result("result").asInstanceOf[HashMap[String, Any]]
          case Some(err) => {
            val error = err.asInstanceOf[HashMap[String, Any]]
            val errorCode = error("error").asInstanceOf[String]
            throw ApiError(errorCode, error)
          }
        }
    }
  }

  /** Simple command to check if node is alive */
  def ping: Future[HashMap[String, Any]] = command("sping")
}

/** Contains connection static methods/helpers */
object NodeConnection {
  /** Calculates contract id by it's BOSS binary
   *
   * @param packed - BOSS encoded contract
   */
  def contractId(packed: Seq[Byte]): Seq[Byte] = HashId(packed).composite3

  /** Common helper for making requests
   *
   * @param method - GET/POST
   * @param url - url string
   * @param data - data to send
   * @param headers - header to set for request
   */
  def request(
    method: String,
    url: String,
    data: HashMap[String, Any] = HashMap[String, Any](),
    headers: HashMap[String, String] = HashMap[String, String]()
  ): Future[HashMap[String, Any]] = {
    val p = Promise[HashMap[String, Any]]()
    val xhr = new dom.XMLHttpRequest()

    xhr.responseType = "arraybuffer"

    xhr.onload = { (e: dom.Event) =>
      val status = xhr.status
      val xhrResponse = (new Uint8Array(xhr.response.asInstanceOf[ArrayBuffer])).toSeq

      if (status >= 200 && status < 300) {
        val result = Boss.load(xhrResponse.asInstanceOf[Seq[Byte]]).asInstanceOf[HashMap[String, Any]]

        if (result("result").asInstanceOf[String] == "ok") {
          result.get("response") match {
            case None => p.failure(ApiError("Node request is ok, but no response key", result))
            case Some(value) => p.success(value.asInstanceOf[HashMap[String, Any]])
          }
        } else {
          p.failure(ApiError("Node request is not ok", result))
        }
      } else {
        p.failure(ApiError("Node request error status", HashMap(
          ("status", status),
          ("text", xhr.statusText)
        )))
      }
    }

    xhr.onerror = { (e: dom.Event) =>
      p.failure(ApiError(HashMap(
        ("status", xhr.status),
        ("text", xhr.statusText)
      )))
    }

    xhr.open(method, url)
    xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")

    for ((k, v) <- headers.asInstanceOf[HashMap[String, String]]) {
      xhr.setRequestHeader(k, v)
    }

    val encodedData = ListBuffer[String]()

    data foreach (x => {
      val dkey = encodeURIComponent(x._1.asInstanceOf[String])
      val dvalue = x._2 match {
        case s: String => encodeURIComponent(s)
        case n: Int => n
      }

      encodedData += s"$dkey=$dvalue"
    })

    xhr.send(encodedData.mkString("&"))

    p.future
  }

  /** Common helper for making BOSS-encoded requests
   *
   * @param url - url string
   * @param data - data to send
   */
  def bossRequest(
    url: String,
    data: HashMap[String, Any]
  ): Future[HashMap[String, Any]] = {
    request("POST", url, HashMap(("requestData64", encode64(Boss.dump(data)))))
  }
}

package cloud

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

import boss._
import boss.jsany._

import tools.universa.ImplicitConverters._

/** Represents message object to exchange between uses through cloud
 *
 * @constructor creates message with given API provider
 * @param service - cloud access API provider
 */
@JSExportTopLevel("CryptoCloud.Message")
class Message(override val service: Api) extends Item(service) {
  var _from: Option[Party] = None
  var _to: Option[Party] = None

  /** Initializes new message */
  override def initialize(): Unit = {
    tag = Some("comm")
  }

  /** Returns text content from private section */
  def text: Option[String] = priv.get("text").asInstanceOf[Option[String]]
  /** Returns message type */
  def messageType: Option[String] = priv.get("type").asInstanceOf[Option[String]]
  /** Returns message additional data */

  @JSExport("payload")
  def payloadJS: js.Any = {
    Boss.write(payload.get)
  }

  def payload: Option[HashMap[String, Any]] = {
    priv.get("payload") match {
      case None => {
        val empty = HashMap[String, Any]()
        setPrivateData("payload", empty)
        Some(empty)
      }
      case Some(value) => Some(value.asInstanceOf[HashMap[String, Any]])
    }
  }

  /** Returns message sender */
  def from: Option[Party] = _from
  /** Returns message recipient */
  def to: Option[Party] = _to

  // FIXME: Linker bug?
  // override def toString: String = {
  //   val displayId = id.getOrElse(null)
  //   val displayFrom = from.getOrElse("null")
  //   val displayType = messageType.getOrElse("null")
  //   val displayText = text.getOrElse("null")

  //   s"$displayId[$displayFrom]:$displayType:$displayText"
  // }

  /** Sends message to recipient */
  def deliver: Future[Message] = {
    val packed = pack

    val transfer = service.execute("connectors_deliver", HashMap(
      ("connector_id", priv("connector_id")),
      ("data", packed)
    ))

    val self = this

    transfer map {
      response => {
        record = response.get("item").asInstanceOf[Option[HashMap[String, Any]]]
        self
      }
    }
  }

  @JSExport("deliver")
  def deliverJS(): js.Promise[Message] = deliver.toJSPromise

  @JSExport("from")
  def fromJS: Party = from.get

  @JSExport("to")
  def toJS: Party = to.get

  @JSExport("type")
  def typeJS: String = messageType.get

  @JSExport("text")
  def textJS: String = text.get

  /** Performs after-load setup in async mode */
  override def setupLoadedData: Future[Unit] = {
    val fromId = readIntSafe(priv("from_party_id"))

    def getFrom: Future[Option[Party]] = {
      _from match {
        case None => service.getParty(HashMap(("partyId", fromId)))
        case Some(party) => Future.successful(Some(party))
      }
    }

    def getConnector(party: Option[Party]): Future[Connector] = {
      _from = party
      service.getConnector(readIntSafe(priv("connector_id")))
    }

    def getTo(c: Connector): Future[Option[Party]] = {
      val toIds = c.parties.map(party => readIntSafe(party("id"))).filter(_ != fromId)
      service.getParty(HashMap(("partyId", toIds.head)))
    }

    def setTo(party: Option[Party]): Future[Unit] = Future { _to = party }

    for {
      fromParty <- getFrom
      connector <- getConnector(fromParty)
      toParty   <- getTo(connector)
      done      <- setTo(toParty)
    } yield done
  }

  /** Packs message to capsule */
  override def toCapsule: Capsule = {
    val cap = super.toCapsule
    to.foreach(party => cap.addEncryptor(party.publicKey))
    cap
  }
}

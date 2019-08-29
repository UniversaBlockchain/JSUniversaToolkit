package xchange

import boss._
import boss.jsany._
import models._
import org.scalajs.dom
import storage.Storage
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._

import scala.collection.mutable.HashMap
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.control.NoStackTrace

@JSExportTopLevel("XChange.ApiError")
class ApiError(
                val message: String = "XChange API error",
                val details: HashMap[String, Any] = HashMap[String, Any]()
              ) extends Exception(message) with NoStackTrace {
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
    new ApiError("XChange API error", details)
  }
}

object XChangeAPI {
  val XCHANGE_HOST = "https://xchange.mainnetwork.io"
  type Dict = HashMap[String, Any]
  type JSDict = js.Dictionary[Any]

  def request(method: String, route: String, params: Dict = HashMap[String, Any]()): Future[Dict] = {
    val p = Promise[Dict]()
    val xhr = new dom.XMLHttpRequest()

    xhr.open(method, s"$XCHANGE_HOST/api/v1/$route")

    xhr.setRequestHeader("Content-Type", "application/json; charset=utf-8")

    xhr.onload = { e: dom.Event =>
      val status = xhr.status
      val xhrResponse = xhr.response.asInstanceOf[String]

      if (status >= 200 && status < 300) {
        val parsed = JSON.parse(xhrResponse).asInstanceOf[js.Any]

        p.success(Boss.read[js.Any](parsed).asInstanceOf[Dict])
      } else {
        p.failure(ApiError(HashMap(
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

    xhr.send(JSON.stringify(params.toJSDictionary))

    p.future
  }

  def getCost(contract: Contract): Future[Dict] = {
    getCost(contract.temp)
  }

  def getCost(cap: Capsule): Future[Dict] = {
    val binary = cap.currentBinary
    val tpack = new TransactionPack(binary)

    getTransactionCost(tpack)
  }

  def getTransactionCost(tpack: TransactionPack): Future[Dict] = {
    val packed = encode64(Boss.dump(tpack))
    val params = HashMap(("packedContract", packed))

    request("POST", "contracts/cost", params.asInstanceOf[HashMap[String, Any]])
  }

  @JSExportTopLevel("XChange.getCost")
  def getCostJS(cap: CapsuleExported): js.Promise[JSDict] = getCost(cap.capsule)

  @JSExportTopLevel("XChange.getTransactionCost")
  def getTransactionCostJS(tpack: TransactionPackExported): js.Promise[JSDict] =
    getTransactionCost(tpack.tp)

  def getNetworkJsDate(): js.Date = {
    //we get current date at first, a bit earlier better than a bit later!
    val localCurrentDate = js.Date.now()
    val offset =
      Storage.timeStorage.getFirst.map(_.localTimeOffsetMillis)
        .getOrElse{
          println("Error: No offset found between local time and universa time!")
          0d
        }
    new js.Date(localCurrentDate + offset)
  }

  private def getNetworkTimeInMillis(): Future[Double] = {
    request("GET", "utc")
      .map(dict => dict("currentEpochSecond").asInstanceOf[Double] * 1000)
  }

  def getTimeOffsetWithUniversaNetwork(): Future[Double] = {
    //taking into account network delays, we should be safe here
    //as offset will be less than actual, so we are in the past
    getNetworkTimeInMillis().map{uTime =>
      val localCurrentDate = js.Date.now()
      uTime - localCurrentDate.toLong
    }
  }
}

class LocalTimeDifference(
                         val localTimeOffsetMillis: Double,
                         val lastCheckTime: Double
                         ) extends BossSerializable {
  override def toJS: LocalTimeDifferenceJS =
    LocalTimeDifferenceJS(localTimeOffsetMillis, lastCheckTime)
}

case class LocalTimeDifferenceJS(
                                localTimeOffsetMillis: Double,
                                lastCheckTime: Double
                              ) extends BossCase

object LocalTimeDifference {
  def fromJS(bossCase: BossCase): BossSerializable = {
    val ltJS = bossCase.asInstanceOf[LocalTimeDifferenceJS]
    new LocalTimeDifference(ltJS.localTimeOffsetMillis, ltJS.lastCheckTime)
  }
}

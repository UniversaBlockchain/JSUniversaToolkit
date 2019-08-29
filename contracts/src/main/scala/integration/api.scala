import org.scalajs.dom

import scala.scalajs.js
import js.JSON
import js.JSConverters._
import js.annotation.{ JSExport, JSExportTopLevel, JSExportStatic }

import scala.collection.mutable.{ HashMap, ListBuffer }

import scala.util.control.NoStackTrace

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import boss._
import boss.jsany._

import tools.universa.UniversaTools._
import tools.universa.ImplicitConverters._

import models.{ Contract, Capsule, PrivateKey, PrivateKeyExported, TransactionPack, TransactionPackExported, ContractExported, RoleLink, RoleSimple }


@JSExportTopLevel("Integration.ApiError")
class ApiError(
  val message: String = "Integration API error",
  val details: HashMap[String, Any] = HashMap[String, Any]()
) extends Exception(message) with NoStackTrace {
  @JSExport("message")
  val messageJS = message

  @JSExport("details")
  val detailsJS = details.toJSDictionary

  override def toString: String = s"Integration API error: $message"
}

object ApiError {
  def apply(message: String, details: HashMap[String, Any]): ApiError = {
    new ApiError(message, details)
  }

  def apply(details: HashMap[String, Any]): ApiError = {
    new ApiError("Integration API error", details)
  }
}

case class RequestData(
  url: String,
  method: String,
  static_params: HashMap[String, Any],
  dynamic_params: HashMap[String, Any]
)

class IntegrationProcessor(val setupURL: String) {
  type Dict = HashMap[String, Any]

  var setupData: Option[Dict] = None
  var okParams: Option[Dict] = None
  var contractsToRegister = ListBuffer[Contract]()

  def setup: Future[Dict] = {
    val loadData = IntegrationProcessor.request("GET", setupURL)

    def setData(data: Dict): Dict = {
      setupData = Some(data)

      data
    }

    loadData.map(setData)
  }

  def acceptInfo: Option[RequestData] = {
    setupData match {
      case None => throw new ApiError("Integration Error: No setup info")
      case Some(data) => {
        val info = data("ok_request").asInstanceOf[Dict]
        val requestData = RequestData(
          info("url").asInstanceOf[String],
          info("method").asInstanceOf[String],
          info("static_params").asInstanceOf[Dict],
          info("dynamic_params").asInstanceOf[Dict]
        )
        Some(requestData)
      }
    }
  }

  def getAction: Option[String] = setupData match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => Some(data("action").asInstanceOf[String])
  }

  def getText: Option[String] = setupData match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => Some(data("text").asInstanceOf[String])
  }

  def getCaption: Option[String] = setupData match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => Some(data("caption").asInstanceOf[String])
  }

  def prepareAcceptParams(
    privateKey: PrivateKey,
    useLongAddress: Boolean = false
  ): Unit = {
    val params = HashMap[String, Any]()
    contractsToRegister = ListBuffer[Contract]()

    for ((name, value) <- acceptInfo.get.static_params) {
      params(name) = value
    }

    for ((name, value) <- acceptInfo.get.dynamic_params) {
      val info = value.asInstanceOf[Dict]
      val dataFormat = info("data_format").asInstanceOf[Dict]
      val paramType = dataFormat("package").asInstanceOf[String]

      params(name) = paramType match {
        case "contract" => prepareContract(dataFormat)
      }
    }

    def prepareContract(dataFormat: Dict): String = {
      val contract = IntegrationProcessor.createContract(dataFormat, privateKey, useLongAddress)

      if (dataFormat("registered").asInstanceOf[Boolean] == true)
        contractsToRegister += contract

      val tpack = TransactionPack(contract.original)
      encode64(tpack.toBOSS)
    }

    okParams = Some(params)
  }

  def accept: Future[Dict] = {
    val ok = acceptInfo.get

    IntegrationProcessor.request(ok.method, ok.url, okParams.get)
  }
}

object IntegrationProcessor {
  type Dict = HashMap[String, Any]
  type JSDict = js.Dictionary[Any]

  def getAssetsLimits: Future[Dict] = {
    request("GET", "https://assets.mainnetwork.io/api/limits")
  }

  def createContract(
    dataFormat: Dict,
    privateKey: PrivateKey,
    useLongAddress: Boolean = false
  ): Contract = {
    val capsule = Capsule(privateKey.publicKey, useLongAddress)

    dataFormat.get("definition") match {
      case Some(definit) => {
        val definition = definit.asInstanceOf[Dict]

        for ((name, value) <- definition) {
          capsule.setDefinition(name, value)
        }

        // FIXME: remove hardcode
        capsule.setDefinition("data.name", "assets key registration contract")
      }
      case None =>
    }

    capsule.lockDataAndSign(privateKey)
    new Contract(capsule.currentBinary)
  }

  def request(
    method: String,
    url: String,
    params: Dict = HashMap[String, Any]()
  ): Future[Dict] = {
    val p = Promise[Dict]()
    val xhr = new dom.XMLHttpRequest()

    xhr.open(method, url)
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

  def runTest: Unit = {
    val setupData = "{\"caption\":\"Select key address to use with ASSETS\",\"text\":\"To use ASSETS service you need to register your key address in the system first.\",\"action\":\"select_private_key_address\",\"ok_request\":{\"url\":\"https://dev-access.universa.io/contracts\",\"method\":\"post\",\"static_params\":{\"auth_token\":\"fiiGtMpYtmAE37934wVO4EXVxbn9LyS6Gl8Fo\",\"operation_token\":\"wzxQM_ucjJvVOp3zcvVrNLjn4Z9U7W3ZTEGUGiK80sEM1eq4QPU\"},\"dynamic_params\":{\"contract_base64\":{\"data_format\":{\"package\":\"contract\",\"registered\":false,\"definition\":{\"name\":\"assets registration contract\",\"description\":\"this contract states the signed intention to register its key with assets service\",\"data\":{\"token\":\"wzxQM_ucjJvVOp3zcvVrNLjn4Z9U7W3ZTEGUGiK80sEM1eq4QPU\"}}}}}},\"status\":\"OK\"}"
    val keyBin = decode64("JgAcAQABvIDzPjpWf3vcSCW1o6CJp4RRgysmSp1GbgxCyMv75GoYtIw+oBPdboWf3o78gcc165fgW+GDjzhnfdQ8rY6fhGl8gV5FUmhmb9aKwcG75914laSQZDNvMi2jzZu5DwvOwk1Qg+6XL3Oa4//9QUgsYu7RUnavJg0nUVXID2KW5VRqk7yA0GhWxMiibwJjfeXiVTlo79MtKpI97NCF1J2m+bP8U+jEp6El/HL0HSa+UJwWm+0CYwTPvvdHZ8HueBJ9N/ig31c8S4ML6NX+3BpBlkUWc3y2TWjzjdKDEy3rbB9I3gJSbhKIg+52Ju+bC19KC0TvYCXSaQH1yFo1eOMJqkS+sXM=")
    val privateKey = new PrivateKey(keyBin)
    val parsed = JSON.parse(setupData).asInstanceOf[js.Any]
    val data = Boss.read[js.Any](parsed).asInstanceOf[Dict]

    val processor = new IntegrationProcessor("http://setuplink")
    processor.setupData = Some(data)

    println(processor.acceptInfo.get.url)
    println(processor.getAction.get)
    processor.prepareAcceptParams(privateKey, true)
    println("==============")
    println(processor.contractsToRegister)
    println(processor.okParams)

    processor.accept
  }
}

@JSExportTopLevel("Universa.IntegrationProcessor")
class IntegrationProcessorExported(val processor: IntegrationProcessor) extends js.Object {
  type JSDict = js.Dictionary[Any]

  def setup(): js.Promise[JSDict] = processor.setup
  def getAction(): String = processor.getAction.get
  def getCaption(): String = processor.getCaption.get
  def getText(): String = processor.getText.get

  def prepareAcceptParams(
    privateKey: PrivateKeyExported,
    useLongAddress: Boolean
  ): Unit = {
    processor.prepareAcceptParams(privateKey.privateKey, useLongAddress)
  }

  def getContractsToRegister(): js.Array[ContractExported] = {
    var list = js.Array[ContractExported]()

    for (con <- processor.contractsToRegister) {
      list += new ContractExported(con)
    }

    list
  }

  def accept(): js.Promise[JSDict] = processor.accept

  def TESTsetup(setupData: String): Unit = {
    val parsed = JSON.parse(setupData).asInstanceOf[js.Any]
    val data = Boss.read[js.Any](parsed).asInstanceOf[HashMap[String, Any]]

    processor.setupData = Some(data)
  }
}

object IntegrationProcessorExported {
  type JSDict = js.Dictionary[Any]

  @JSExportStatic("create")
  def apply(setupURL: String): IntegrationProcessorExported = {
    val processor = new IntegrationProcessor(setupURL)
    new IntegrationProcessorExported(processor)
  }

  @JSExportStatic
  def runTest(): Unit = IntegrationProcessor.runTest

  @JSExportStatic
  def getAssetsLimits(): js.Promise[JSDict] = IntegrationProcessor.getAssetsLimits
}

class Emission(
  val privateKey: PrivateKey,
  val useLongAddress: Boolean,
  val currency: String,
  val amount: String
) {
  type Dict = HashMap[String, Any]

  val capsule = Capsule(privateKey.publicKey, useLongAddress)
  capsule.setDefinition("data.name", "assets_emission_request")
  capsule.setDefinition("data.currency", currency)
  capsule.setDefinition("data.amount", amount)
  capsule.lockDataAndSign(privateKey)

  val contract = new Contract(capsule.currentBinary)

  var id: Option[Int] = None

  def request: Future[Dict] = {
    val tpack = TransactionPack(contract.original)
    val tpack64 = encode64(tpack.toBOSS)
    val params = HashMap(("contract_base64", tpack64)).asInstanceOf[Dict]
    val req = IntegrationProcessor.request("POST", "https://assets.mainnetwork.io/api/emissions", params)

    req.map(response => {
      val emission = response("emission").asInstanceOf[Dict]
      id = Some(emission("id").asInstanceOf[Int])
      response
    })
  }

  def confirm(code: String = "1101"): Future[Dict] = {
    val realId = id.get
    val url = s"https://assets.mainnetwork.io/api/emissions/$realId/confirm"
    val params = HashMap(("sms_code", code)).asInstanceOf[Dict]
    IntegrationProcessor.request("POST", url, params)
  }

  def getState: Future[Dict] = {
    val realId = id.get
    val url = s"https://assets.mainnetwork.io/api/emissions/$realId"

    IntegrationProcessor.request("GET", url)
  }
}

object Emission {
  type Dict = HashMap[String, Any]

  def getState(id: Int): Future[Dict] = {
    val url = s"https://assets.mainnetwork.io/api/emissions/$id"

    IntegrationProcessor.request("GET", url)
  }

  def confirm(id: Int, code: String = "1101"): Future[Dict] = {
    val url = s"https://assets.mainnetwork.io/api/emissions/$id/confirm"
    val params = HashMap(("sms_code", code)).asInstanceOf[Dict]
    IntegrationProcessor.request("POST", url, params)
  }
}

@JSExportTopLevel("Universa.Emission")
class EmissionExported(val emission: Emission) extends js.Object {
  type JSDict = js.Dictionary[Any]

  def request(): js.Promise[JSDict] = emission.request
  def confirm(code: String = "1101"): js.Promise[JSDict] = emission.confirm(code)
  def getId(): Int = emission.id.get
  def getState(): js.Promise[JSDict] = emission.getState
}

object EmissionExported {
  type JSDict = js.Dictionary[Any]

  @JSExportStatic("create")
  def apply(
    privateKey: PrivateKeyExported,
    useLongAddress: Boolean,
    currency: String,
    amount: String
  ): EmissionExported = {
    val emission = new Emission(privateKey.privateKey, useLongAddress, currency, amount)
    new EmissionExported(emission)
  }

  @JSExportStatic
  def getState(id: Int): js.Promise[JSDict] = Emission.getState(id)

  @JSExportStatic
  def confirm(id: Int, code: String = "1101"): js.Promise[JSDict] = Emission.confirm(id, code)
}

class Withdrawal(
  val contract: Contract,
  val privateKey: PrivateKey,
  val details: HashMap[String, Any]
) {
  type Dict = HashMap[String, Any]

  var id: Option[Int] = None

  def prepare: TransactionPack = {
    contract.revertOriginal
    contract.createRevision

    val oldOwner = contract.temp.getRole("owner").get.asInstanceOf[RoleSimple]
    val creator = oldOwner.copyWithName("creator")

    val issuer = contract.temp.getRole("issuer").get.asInstanceOf[RoleSimple]
    // val owner = new RoleLink("owner", "issuer")
    val owner = issuer.copyWithName("owner")
    contract.temp.setState("owner", owner)
    contract.temp.setState("created_by", creator)
    contract.temp.resetTransactional
    contract.temp.setTransactional("data", details)
    contract.temp.setTransactional("data.purpose", "withdrawal")
    contract.temp.lockDataAndSign(privateKey)

    contract.getTransactionPack
  }

  def send(contract: Contract): Future[Dict] = {
    val tpack = TransactionPack(contract.temp)
    val tpack64 = encode64(tpack.toBOSS)
    val params = HashMap(("contract_base64", tpack64)).asInstanceOf[Dict]
    val req = IntegrationProcessor.request("POST", "https://assets.mainnetwork.io/api/withdrawals", params)

    req.map(response => {
      val withdrawal = response("withdrawal").asInstanceOf[Dict]
      id = Some(withdrawal("id").asInstanceOf[Int])
      response
    })
  }

  def getState: Future[Dict] = {
    val realId = id.get
    val url = s"https://assets.mainnetwork.io/api/withdrawals/$realId"

    IntegrationProcessor.request("GET", url)
  }
}

object Withdrawal {
  type Dict = HashMap[String, Any]

  def apply(contract: Contract, privateKey: PrivateKey): Withdrawal = {
    new Withdrawal(contract, privateKey, HashMap[String, Any]())
  }

  def getState(id: Int): Future[Dict] = {
    val url = s"https://assets.mainnetwork.io/api/withdrawals/$id"

    IntegrationProcessor.request("GET", url)
  }
}

@JSExportTopLevel("Universa.Withdrawal")
class WithdrawalExported(val withdrawal: Withdrawal) extends js.Object {
  type JSDict = js.Dictionary[Any]

  def prepare(): TransactionPackExported =
    new TransactionPackExported(withdrawal.prepare)
  def send(contract: ContractExported): js.Promise[JSDict] = withdrawal.send(contract.contract)
  def getId(): Int = withdrawal.id.get
  def getState(): js.Promise[JSDict] = withdrawal.getState
}

object WithdrawalExported {
  type JSDict = js.Dictionary[Any]

  @JSExportStatic("create")
  def apply(
    contract: ContractExported,
    privateKey: PrivateKeyExported,
    details: js.Dictionary[Any]
  ): WithdrawalExported = {
    val withdrawal = new Withdrawal(
      contract.contract,
      privateKey.privateKey,
      details
    )

    new WithdrawalExported(withdrawal)
  }

  @JSExportStatic("createByContract")
  def apply(
    contract: ContractExported,
    privateKey: PrivateKeyExported
  ): WithdrawalExported = {
    val withdrawal = Withdrawal(
      contract.contract,
      privateKey.privateKey
    )

    new WithdrawalExported(withdrawal)
  }

  @JSExportStatic
  def getState(id: Int): js.Promise[JSDict] = Withdrawal.getState(id)
}

@JSExportTopLevel("Universa.UTNDelivery")
class UTNDeliveryExported(val delivery: UTNDelivery) extends js.Object {
  type JSDict = js.Dictionary[Any]

  def setup(): js.Promise[JSDict] = delivery.setup

  def getAction(): String = delivery.getAction.get
  def getCaption(): String = delivery.getCaption.get
  def getText(): String = delivery.getText.get

  def accept(
    privateKey: PrivateKeyExported,
    useLongAddress: Boolean
  ): js.Promise[JSDict] = {
    delivery.accept(privateKey.privateKey, useLongAddress)
  }

  def getCheckURL(): String = delivery.getCheckURL
  def getState(): js.Promise[JSDict] = delivery.getState
}

object UTNDeliveryExported {
  @JSExportStatic("create")
  def apply(setupURL: String): UTNDeliveryExported = {
    val delivery = new UTNDelivery(setupURL)
    new UTNDeliveryExported(delivery)
  }

  @JSExportStatic("createFromCheckURL")
  def restore(setupURL: String): UTNDeliveryExported = {
    val delivery = UTNDelivery.fromCheckURL(setupURL)
    new UTNDeliveryExported(delivery)
  }
}

class UTNDelivery(val setupURL: String) {
  type Dict = HashMap[String, Any]

  var orderInfo: Option[Dict] = None

  def setup: Future[Dict] = {
    val loadData = IntegrationProcessor.request("GET", setupURL)

    def setData(data: Dict): Dict = {
      orderInfo = Some(data)

      data
    }

    loadData.map(setData)
  }

  def setOrderInfo(info: Dict): Unit = orderInfo = Some(info)

  def accept(
    privateKey: PrivateKey,
    useLongAddress: Boolean = false
  ): Future[Dict] = {
    orderInfo match {
      case None => throw new ApiError("Integration Error: No setup info")
      case Some(data) => {
        val address = if (useLongAddress) privateKey.publicKey.longAddress else privateKey.publicKey.shortAddress
        val address58 = encode58(address)

        IntegrationProcessor.request("POST", data("ok_url").asInstanceOf[String], HashMap(("address", address58)))
      }
    }
  }

  def getAction: Option[String] = orderInfo match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => Some(data("action").asInstanceOf[String])
  }

  def getText: Option[String] = orderInfo match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => Some(data("text").asInstanceOf[String])
  }

  def getCaption: Option[String] = orderInfo match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => Some(data("caption").asInstanceOf[String])
  }

  def getState: Future[Dict] = {
    orderInfo match {
      case None => throw new ApiError("Integration Error: No setup info")
      case Some(data) => IntegrationProcessor.request("GET", data("check_url").asInstanceOf[String])
    }
  }

  def getCheckURL: String = orderInfo match {
    case None => throw new ApiError("Integration Error: No setup info")
    case Some(data) => data("check_url").asInstanceOf[String]
  }
}

object UTNDelivery {
  def fromCheckURL(checkURL: String): UTNDelivery = {
    val delivery = new UTNDelivery(checkURL)
    delivery.setOrderInfo(HashMap(("check_url", checkURL)))
    delivery
  }
}

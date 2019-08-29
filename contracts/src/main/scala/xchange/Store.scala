package xchange

import boss.jsany._
import boss.{Boss, BossCase, BossSerializable}
import cloud.{Api, Item}
import models._
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._
import xchange.XChangeAPI._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.util.Try

object Store {
  type Dict = mutable.HashMap[String, Any]
  type JSDict = js.Dictionary[Any]

  def getProducts: Future[Dict] =
    request("GET", "products/list?filter=tu", mutable.HashMap[String, Any]())

  @JSExportTopLevel("XChange.store.getProducts")
  def getProductsJS(): js.Promise[JSDict] = getProducts

  @JSExportTopLevel("XChange.store.getConversions")
  def getConversionsJS(): js.Promise[JSDict] =
    request("GET", "conversions", mutable.HashMap[String, Any]())

  @JSExportTopLevel("XChange.utnp.getCommission")
  def getCommissionUTNPJS(): js.Promise[JSDict] =
    request("GET", "utnp/import/commission", mutable.HashMap[String, Any]())

  @JSExportTopLevel("XChange.utn.getLimits")
  def getCommissionUTNJS(): js.Promise[JSDict] =
    request("GET", "utn/export/limits", mutable.HashMap[String, Any]())

  @JSExportTopLevel("XChange.utn.u.getLimits")
  def getCommissionUJS(): js.Promise[JSDict] =
    request("GET", "uutn/info", mutable.HashMap[String, Any]())
}

object OrderTypes {
  val productType: String = "product"
  val conversionType: String = "conversion"
  val UTNP_UTN_Type: String = "UTNP-UTN"
  val UTN_UTNP_Type: String = "UTN-UTNP"
  val UTN_U_Type: String = "UTN-U"
}

case class OrderJS(
                    properties: mutable.HashMap[String, String],
                    `type`: String = OrderTypes.productType
                  ) extends BossCase

class Order(
             var orderCode: Option[String] = None,
             val currency: Option[String],
             val quantity: Option[Int],
             val amount: Option[String],
             val publicKey: Option[PublicKey],
             val productCode: Option[String],
             val returnAddress: Option[String],
             val promocode: Option[String],
             var compound: Option[Seq[Byte]],
             val `type`: String
           ) extends BossSerializable {
  this: AbstractOrderApi =>

  override def toJS: OrderJS = {
    val optionals = new scala.collection.mutable.HashMap[String, String]()

    orderCode.foreach(code => optionals.put("orderCode", code))
    currency.foreach(cur => optionals.put("currency", cur))
    quantity.foreach(quantt => optionals.put("quantity", quantt.toString))
    amount.foreach(am => optionals.put("amount", am))
    publicKey.foreach(pub => optionals.put("publicKey", encode64(pub.toBOSS)))
    productCode.foreach(code => optionals.put("productCode", code))
    returnAddress.foreach(addr => optionals.put("returnAddress", addr))
    promocode.foreach(code => optionals.put("promocode", code))
    compound.foreach(comp => optionals.put("compound", encode64(comp)))

    OrderJS(optionals, `type`)
  }
}

@JSExportTopLevel("XChange.Order")
class OrderExported(val order: Order with AbstractOrderApi) extends CloudElement {
  def send(): js.Promise[JSDict] = order.send

  def prepare(): js.Promise[JSDict] = order.prepare

  def prepare(address: js.Array[Byte]): js.Promise[JSDict] = {
    val orderType = order.`type`

    if (orderType == OrderTypes.UTN_U_Type)
      order.asInstanceOf[UTN_UApi].prepareForAddress(address.toSeq)
    else throw new RuntimeException(s"method is not available for $orderType order")
  }

  def sign(privateKey: PrivateKeyExported): TransactionPackExported =
    new TransactionPackExported(order.sign(privateKey.privateKey))

  def getState(): js.Promise[JSDict] = order.getState

  def download(): js.Promise[JSDict] = order.download

  def orderCode: String = order.orderCode.orNull

  def productCode: String = order.productCode.orNull
}

object OrderExported extends CloudFunc[OrderExported] {
  val ItemTag: String = "Order"

  @JSExportStatic("createExport")
  def apply(params: js.Dictionary[Any]): OrderExported = {
    apply(null, params)
  }

  @JSExportStatic("createExportByCompound")
  def apply(compoundTP: TransactionPackExported, addr: String): OrderExported = {
    val tpack = compoundTP.tp

    val order = buildOrder(
      None, None, None, None, None, None,
      Some(addr), None, Some(tpack.toBOSS), "UTN-UTNP"
    )

    new OrderExported(order)
  }

  @JSExportStatic("createUPurchaseByCompound")
  def createUTNU(compoundTP: TransactionPackExported, amount: String): OrderExported = {
    val tpack = compoundTP.tp

    val order = buildOrder(
      None, None, None, None, None, Some(amount),
      None, None, Some(tpack.toBOSS), "UTN-U"
    )

    new OrderExported(order)
  }

  @JSExportStatic("create")
  def apply(publicKey: PublicKeyExported, params: js.Dictionary[Any]): OrderExported = {
    val orderTypeOpt = params.get("type").asInstanceOf[Option[String]]
    val orderType = orderTypeOpt.getOrElse(OrderTypes.productType)

    val productCode = params.get("productCode").asInstanceOf[Option[String]]
    if (orderType == OrderTypes.productType && productCode.isEmpty) {
      throw new ApiError("productCode must be defined")
    }

    val currency = params.get("currency").asInstanceOf[Option[String]]
    if (
      orderType != OrderTypes.UTNP_UTN_Type &&
        orderType != OrderTypes.UTN_UTNP_Type &&
        orderType != OrderTypes.UTN_U_Type &&
        currency.isEmpty
    ) {
      throw new ApiError("currency must be defined")
    }

    val amount = params.get("amount").asInstanceOf[Option[String]]
    if (orderType == OrderTypes.conversionType && amount.isEmpty) {
      throw new ApiError("amount for conversion must be defined")
    }

    val quantity = params.get("quantity").asInstanceOf[Option[Int]]
    if (orderType == OrderTypes.productType && !quantity.contains(1)) {
      throw new ApiError("currently only 1 is allowed for quantity value")
    }

    val returnAddress = params.get("returnAddress").asInstanceOf[Option[String]]
    if (orderType == OrderTypes.UTN_UTNP_Type && returnAddress.isEmpty) {
      throw new ApiError("return address must be defined")
    }

    val promocode = params.get("promocode").asInstanceOf[Option[String]]
    val compound = params.get("compound").asInstanceOf[Option[Seq[Byte]]]

    val order = buildOrder(
      None, Option(publicKey).map(_.publicKey), productCode, currency, quantity, amount,
      returnAddress, promocode, compound, orderType
    )

    new OrderExported(order)
  }

  //create builder class?
  private def buildOrder(
                          orderCode: Option[String],
                          publicKey: Option[PublicKey],
                          productCode: Option[String],
                          currency: Option[String],
                          quantity: Option[Int],
                          amount: Option[String],
                          returnAddress: Option[String],
                          promocode: Option[String],
                          compound: Option[Seq[Byte]],
                          orderType: String
                        ) = {
    orderType match {
      case OrderTypes.productType => new Order(
        orderCode, currency, quantity, amount,
        publicKey, productCode, returnAddress, promocode, None, orderType) with ProductOrderApi

      case OrderTypes.conversionType => new Order(
        orderCode, currency, quantity, amount,
        publicKey, None, returnAddress, None, None, orderType) with ConversionApi

      case OrderTypes.UTNP_UTN_Type => new Order(
        orderCode, currency, None, amount,
        publicKey, None, None, None, None, orderType
      ) with UTNP_UTNApi

      case OrderTypes.UTN_UTNP_Type => new Order(
        orderCode, None, None, amount,
        None, None, returnAddress, None, compound, orderType
      ) with UTN_UTNPApi

      case OrderTypes.UTN_U_Type => new Order(
        orderCode, None, None, amount,
        None, None, None, None, compound, orderType
      ) with UTN_UApi
    }
  }

  def apply(orderJS: OrderJS): Order = {
    val publicKey = orderJS.properties.get("publicKey") map {
      encoded => Boss.load(decode64(encoded))
    }

    val compound = orderJS.properties.get("compound") map {
      decode64
    }

    buildOrder(
      orderJS.properties.get("orderCode"),
      publicKey.asInstanceOf[Option[PublicKey]],
      orderJS.properties.get("productCode"),
      orderJS.properties.get("currency"),
      orderJS.properties.get("quantity").map(_.toInt),
      orderJS.properties.get("amount"),
      orderJS.properties.get("returnAddress"),
      orderJS.properties.get("promocode"),
      compound.asInstanceOf[Option[Seq[Byte]]],
      orderJS.`type`
    )
  }

  def fromJS(serialized: BossCase): BossSerializable = {
    apply(serialized.asInstanceOf[OrderJS])
  }

  @JSExportStatic("getState")
  def getStateJS(orderCode: String, `type`: String = OrderTypes.productType): js.Promise[JSDict] = {
    `type` match {
      case OrderTypes.productType => request("GET", s"products/orders/$orderCode/state")
      case OrderTypes.conversionType => request("GET", s"conversions/$orderCode/state")
      case OrderTypes.UTNP_UTN_Type => request("GET", s"utnp/import/$orderCode")
      case OrderTypes.UTN_UTNP_Type => request("GET", s"utn/export/$orderCode")
      case OrderTypes.UTN_U_Type => request("GET", s"uutn/$orderCode")
    }
  }

  @JSExportStatic("getStateByContract")
  def getStateByContractJS(
                            contractBin: String,
                            `type`: String = OrderTypes.UTN_UTNP_Type
                          ): js.Promise[JSDict] = {
    `type` match {
      case OrderTypes.UTN_UTNP_Type =>
        val params = mutable.HashMap[String, Any](
          ("utn_base64", contractBin)
        )

        request("POST", "utn/export", params) map {
          response =>
            // println(response)
            val status = response("status").asInstanceOf[String]
            if (status != "OK")
              throw new ApiError(response("code").asInstanceOf[String], response)

            response
        }
    }
  }

  @JSExportStatic("download")
  def downloadJS(orderCode: String, `type`: String = OrderTypes.productType): js.Promise[JSDict] = {
    `type` match {
      case OrderTypes.productType => request("GET", s"products/orders/$orderCode")
      case OrderTypes.conversionType => request("GET", s"conversions/$orderCode")
      case OrderTypes.UTNP_UTN_Type => request("GET", s"utnp/import/$orderCode")
    }

  }

  @JSExportStatic("ToCloud")
  def uploadToCloudJS(order: OrderExported, api: Api): js.Promise[Double] = {
    var item: Item = null

    item = new Item(api)
    item.setTag(ItemTag)
    item.setPrivateData("transaction", encode64(Boss.dump(order.order)))
    item.save.map(_ => item.id.get.toDouble).toJSPromise
  }

  @JSExportStatic("FromCloud")
  def downloadFromCloudJS(itemId: Double, api: Api): js.Promise[OrderExported] = {
    val loaded = api.getItem(
      { api => new Item(api) },
      mutable.HashMap[String, Any]("id" -> itemId)
    )
    loaded
      .map(item => apply(item))
      .toJSPromise
  }

  def apply(item: Item): OrderExported = {
    val tr = item.priv("transaction").asInstanceOf[String]
    val loaded = Boss.load(decode64(tr))
    val order = Try {
      loaded.asInstanceOf[Order]
    }.getOrElse(throw new RuntimeException(s"No object by item $item found"))
    order.`type` match {
      case OrderTypes.productType =>
        new OrderExported(loaded.asInstanceOf[Order with ProductOrderApi])
      case OrderTypes.conversionType =>
        new OrderExported(loaded.asInstanceOf[Order with ConversionApi])
    }
  }
}

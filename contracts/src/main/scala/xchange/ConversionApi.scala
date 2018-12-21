package xchange

import tools.universa.UniversaTools.encode64
import xchange.XChangeAPI.{Dict, request}

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import models.{ PrivateKey, TransactionPack }

trait ConversionApi extends AbstractOrderApi { this: Order =>

  override def sign(privateKey: PrivateKey): TransactionPack = throw new ApiError("Not implemented")
  override def prepare: Future[Dict] = throw new ApiError("Not implemented")

  override def send: Future[Dict] = {
    val params = HashMap[String, Any](
      ("currency", currency.get),
      ("amount", amount.get),
      ("publicKey", encode64(publicKey.get.pack))
    )

    returnAddress match {
      case Some(addr) => params("returnAddress") = addr
      case None =>
    }

    request("POST", "conversions", params) flatMap {
      response => Future {
        if (!response.contains("orderState")) println(response)
        val state = response("orderState").asInstanceOf[HashMap[String, Any]]
        orderCode = state.get("orderCode").asInstanceOf[Option[String]]

        response
      }
    }
  }

  override def getState: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"conversions/$code/state")
      case None => throw new ApiError("order code is empty")
    }
  }

  override def download: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"conversions/$code")
      case None => throw new ApiError("order code is empty")
    }
  }
}

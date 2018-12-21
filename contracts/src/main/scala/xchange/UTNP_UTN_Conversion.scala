package xchange

import tools.universa.UniversaTools.{ encode64, encode58 }
import xchange.XChangeAPI.{Dict, request}

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import models.{ PrivateKey, TransactionPack }

trait UTNP_UTNApi extends AbstractOrderApi { this: Order =>

  override def sign(privateKey: PrivateKey): TransactionPack = throw new ApiError("Not implemented")
  override def prepare: Future[Dict] = throw new ApiError("Not implemented")

  override def send: Future[Dict] = {
    val params = HashMap[String, Any](
      ("amount", amount.get),
      ("address", encode58(publicKey.get.shortAddress))
    )

    request("POST", "utnp/import", params) map {
      response =>
        if (!response.contains("utnpImport")) println(response)
        val state = response("utnpImport").asInstanceOf[HashMap[String, Any]]
        orderCode = state.get("code").asInstanceOf[Option[String]]

        response
    }
  }

  override def getState: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"utnp/import/$code")
      case None => throw new ApiError("order code is empty")
    }
  }

  override def download: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"utnp/import/$code")
      case None => throw new ApiError("order code is empty")
    }
  }
}

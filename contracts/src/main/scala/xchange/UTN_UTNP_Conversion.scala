package xchange

import boss._
import boss.jsany._
import models.{Contract, PrivateKey, TransactionPack}
import tools.universa.UniversaTools.{decode64, encode64}
import xchange.XChangeAPI.{Dict, request}

import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait UTN_UTNPApi extends AbstractOrderApi {
  this: Order =>

  override def send: Future[Dict] = {
    val tpack = Boss.load(compound.get).asInstanceOf[TransactionPack]

    val params = HashMap[String, Any](
      ("utn_base64", encode64(tpack.contract))
    )

    request("POST", "utn/export", params) map {
      response =>
        val status = response("status").asInstanceOf[String]
        if (status != "OK")
          throw new ApiError(response("code").asInstanceOf[String], response)

        response
    }
  }

  override def getState: Future[Dict] = {
    send
  }

  override def download: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"utnp/import/$code")
      case None => throw new ApiError("order code is empty")
    }
  }

  override def prepare: Future[Dict] = {
    val params = HashMap[String, Any](
      ("receiving_ethereum_address", returnAddress.get),
      ("utn_base64", encode64(compound.get))
    )

    request("POST", "utn/export/prepare", params) map {
      response =>
          val status = response("status").asInstanceOf[String]
          if (status != "OK")
            throw new ApiError(response("code").asInstanceOf[String], response)

          val tpackBin = decode64(response("payment_base64").asInstanceOf[String])

          val originalUTN = new Contract(compound.get).original
          val tpack = Boss.load(tpackBin).asInstanceOf[TransactionPack]
          val newUTN = tpack.mainCapsule

          val amountEquals = originalUTN.getAmount.compare(newUTN.getAmount) == 0
          val addressEquals = newUTN.contract.get("transactional.data.receiving_ethereum_address") == returnAddress.get
          val infoEquals = newUTN.contract.get("transactional.data.payment_info") == "conversion to UTNP"

          if (!amountEquals || !addressEquals || !infoEquals) throw new ApiError("new UTN revision is broken")

          compound = Some(tpackBin)

          response
    }
  }

  override def sign(privateKey: PrivateKey): TransactionPack = {
    val tpack = Boss.load(compound.get).asInstanceOf[TransactionPack]
    val cap = tpack.mainCapsule
    cap.sign(privateKey)
    cap.lock
    tpack.contract = cap.currentBinary

    compound = Some(tpack.toBOSS)
    tpack
  }
}

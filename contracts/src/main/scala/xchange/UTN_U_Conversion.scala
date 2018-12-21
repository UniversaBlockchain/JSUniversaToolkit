package xchange

import boss._
import boss.jsany._
import models.{Contract, Capsule, PrivateKey, TransactionPack}
import models.contracts.ContractFactory
import tools.universa.UniversaTools.{decode64, encode64}
import xchange.XChangeAPI.{Dict, request}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait UTN_UApi extends AbstractOrderApi {
  this: Order =>

  override def send: Future[Dict] = {
    val tpack = Boss.load(compound.get).asInstanceOf[TransactionPack]

    val params = HashMap[String, Any](
      ("compound_base64", encode64(compound.get))
    )

    request("POST", "uutn/purchase", params) map {
      response =>
        val status = response("status").asInstanceOf[String]
        if (status != "OK")
          throw new ApiError(response("code").asInstanceOf[String], response)

        val state = response("purchase").asInstanceOf[HashMap[String, Any]]
        orderCode = state.get("id").asInstanceOf[Option[String]]

        response
    }
  }

  override def getState: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"uutn/$code")
      case None => throw new ApiError("order code is empty")
    }
  }

  override def download: Future[Dict] = {
    orderCode match {
      case Some(code) => request("GET", s"uutn/$code")
      case None => throw new ApiError("order code is empty")
    }
  }

  override def prepare: Future[Dict] = {
    val utnCapsule = new Capsule(compound.get)
    val utnContract = ContractFactory.buildFromCapsule(utnCapsule)
    val utnTransaction = utnContract.getTransactionPack
    val utnRoles = utnCapsule.getRoles.values.toSeq
    val owner = utnCapsule.getRole("owner").get

    val params = HashMap[String, Any](
      ("owner_address", owner.getAddress58(false, mutable.Seq(utnRoles: _*))),
      ("amount", amount.get.toInt),
      ("utn_base64", encode64(utnTransaction.toBOSS))
    )

    request("POST", "uutn/create_purchase", params) map {
      response =>
        val status = response("status").asInstanceOf[String]
        if (status != "OK")
          throw new ApiError(response("code").asInstanceOf[String], response)

        val tpackBin = decode64(response("compound_base64").asInstanceOf[String])

        compound = Some(tpackBin)

        response
    }
  }

  override def sign(privateKey: PrivateKey): TransactionPack = {
    val tpack = Boss.load(compound.get).asInstanceOf[TransactionPack]
    val cap = tpack.mainContract.original
    cap.sign(privateKey)
    cap.lock
    tpack.contract = cap.currentBinary

    compound = Some(tpack.toBOSS)
    tpack
  }
}

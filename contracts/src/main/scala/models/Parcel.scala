package models

import boss._
import boss.jsany._
import models.contracts.UPackContractExported
import models.errors.NoKeyError
import node.NodeApiExported
import tools.universa.ImplicitConverters._
import tools.universa.UniversaTools._

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

case class ParcelJS(
  payload: Seq[Byte],
  payment: Seq[Byte],
  hashId: HashId
) extends BossCase

@JSExportTopLevel("Universa.Parcel")
class ParcelExported(val parcel: Parcel) extends js.Object {
  def register(api: NodeApiExported): js.Promise[js.Dictionary[Any]] = {
    val packedParcel = Boss.dump(parcel)
    api.nodeApi.registerParcelFinal(packedParcel)
  }
}

class Parcel(
  val payloadPack: Seq[Byte],
  val paymentPack: Seq[Byte],
  val hashId: HashId
) extends BossSerializable {

  def toJS: ParcelJS = ParcelJS(payloadPack, paymentPack, hashId)

  def this(loaded: ParcelJS) = this(loaded.payload, loaded.payment, loaded.hashId)
}

object ParcelExported {
  def apply(
    payloadPack: TransactionPack,
    paymentPack: TransactionPack
  ): ParcelExported = {
    val payloadPackBin = Boss.dump(payloadPack)
    val paymentPackBin = Boss.dump(paymentPack)

    val hashId = getHashId(payloadPackBin, paymentPackBin)

    val parcel = new Parcel(payloadPackBin, paymentPackBin, hashId)
    new ParcelExported(parcel)
  }

  @JSExportStatic("create")
  def apply(
    payloadPack: TransactionPackExported,
    upackExp: UPackContractExported,
    cost: Int,
    keys: js.Array[PrivateKeyExported],
    testMode: Boolean = false
  ): ParcelExported = {
    if (cost < 1) throw new RuntimeException("cost can't be less than 1 U")
    val upack = upackExp.contract
    upack.createPayment(cost, testMode)

    val paymentSignerOpt = upack.original.getKeyForRole("owner", keys.map(_.privateKey))
    val paymentSigner = paymentSignerOpt.getOrElse(
      throw new NoKeyError("There's no key for payment owner")
    )

    upack.temp.lockDataAndSign(paymentSigner)

    val paymentBinary = upack.temp.currentBinary

    upack.pending = Some(new Capsule(paymentBinary))

    val paymentPack = new TransactionPack(paymentBinary)
    paymentPack.setSubItems(upack.original.currentBinary)

    ParcelExported(payloadPack.tp, paymentPack)
  }

  private def getHashId(payload: Seq[Byte], payment: Seq[Byte]): HashId = {
    val payloadId = HashId(payload).composite3
    val paymentId = HashId(payment).composite3

    val result = new ListBuffer[Byte]
    result ++= paymentId
    result ++= payloadId

    HashId(ensureBytes(result))
  }

  def fromJS(serialized: BossCase): BossSerializable =
    new Parcel(serialized.asInstanceOf[ParcelJS])
}

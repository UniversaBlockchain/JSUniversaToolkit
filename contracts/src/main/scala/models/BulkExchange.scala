package models

import boss._
import boss.jsany._

import tools.universa.UniversaTools.shortId

import scala.scalajs.js
import js.JSConverters._
import js.annotation.{JSExportStatic, JSExportTopLevel}

import scala.collection.mutable.{ ListBuffer, HashMap }

case class BulkExchangeJS(
  capsulesIn: ListBuffer[Seq[Byte]],
  capsulesOut: ListBuffer[Seq[Byte]],
  capsulesExtra: ListBuffer[Seq[Byte]],
  capsulesRevoking: ListBuffer[Seq[Byte]]
) extends BossCase

@JSExportTopLevel("Universa.BulkExchange")
class BulkExchangeExported(val be: BulkExchange) extends js.Object {
  def this() = this(new BulkExchange)

  def toBOSS(): js.Array[Byte] = Boss.dump(be).toJSArray

  def getIncoming(): js.Array[CapsuleExported] = {
    be.capsulesIn.map(new CapsuleExported(_)).toJSArray
  }

  def getOutcoming(): js.Array[CapsuleExported] = {
    be.capsulesOut.map(new CapsuleExported(_)).toJSArray
  }

  def addIncoming(contract: ContractExported): Unit = {
    be.capsulesIn += contract.temp.capsule
    be.capsulesExtra += contract.original.capsule
  }

  def addOutcoming(contract: ContractExported): Unit = {
    be.capsulesOut += contract.temp.capsule
    be.capsulesExtra += contract.original.capsule
  }

  def addIncomingTP(tp: TransactionPackExported): Unit = {
    val tpack = tp.tp
    val capsule = tpack.mainCapsule
    // remove signatures
    capsule.resetSignatures
    be.capsulesIn += capsule
    addTPReferences(tpack)
  }

  def addOutcomingTP(tp: TransactionPackExported): Unit = {
    val tpack = tp.tp
    val capsule = tpack.mainCapsule
    // remove signatures
    capsule.resetSignatures
    be.capsulesOut += capsule
    addTPReferences(tpack)
  }

  private def addTPReferences(tpack: TransactionPack): Unit = {
    be.capsulesExtra ++= tpack.getNewItems
    be.capsulesExtra ++= tpack.getRevokingItems
    // be.capsulesRevoking ++= tpack.getRevokingContracts().map(_.original)
  }

  def signStep1(privateKeys: js.Array[PrivateKeyExported]): Unit =
    be.signStep1(privateKeys.map(_.privateKey))

  def signStep2(privateKeys: js.Array[PrivateKeyExported]): Unit =
    be.signStep2(privateKeys.map(_.privateKey))

  def signStep3(privateKeys: js.Array[PrivateKeyExported]): Unit =
    be.signStep3(privateKeys.map(_.privateKey))

  def createTransactionPack(key: PrivateKeyExported): TransactionPackExported =
    new TransactionPackExported(be.getTransactionPack(key.privateKey))
}

object BulkExchangeExported {
  @JSExportStatic("fromBOSS")
  def fromBOSS(serialized: js.Array[Byte]): BulkExchangeExported = {
    new BulkExchangeExported(Boss.load(serialized.toSeq).asInstanceOf[BulkExchange])
  }
}

object BulkExchange {
  def fromJS(serialized: BossCase): BossSerializable = {
    new BulkExchange(serialized.asInstanceOf[BulkExchangeJS])
  }
}

class BulkExchange extends BossSerializable {
  var capsulesIn = ListBuffer.empty[Capsule]
  var capsulesOut = ListBuffer.empty[Capsule]
  var capsulesExtra = ListBuffer.empty[Capsule]
  var capsulesRevoking = ListBuffer.empty[Capsule]

  def toJS: BulkExchangeJS = {
    BulkExchangeJS(
      capsulesIn.map(_.currentBinary),
      capsulesOut.map(_.currentBinary),
      capsulesExtra.map(_.currentBinary),
      capsulesRevoking.map(_.currentBinary)
    )
  }

  def this(serialized: BulkExchangeJS) = {
    this()
    capsulesIn = serialized.capsulesIn.map(new Capsule(_))
    capsulesOut = serialized.capsulesOut.map(new Capsule(_))
    capsulesExtra = serialized.capsulesExtra.map(new Capsule(_))
    capsulesRevoking = serialized.capsulesRevoking.map(new Capsule(_))
  }

  private def createTransaction(cap: Capsule) = {
    cap.resetTransactional
    cap.setTransactional("id", shortId)
  }

  def signStep1(keys: Seq[PrivateKey]): Unit = {
    capsulesIn.foreach(cap => createTransaction(cap))
    capsulesOut.foreach(cap => createTransaction(cap))

    capsulesOut.foreach(capsuleOut => {
      capsulesIn.foreach(capsuleIn => {
        val ref = new Reference

        ref.transactionalId = capsuleIn.getTransactional("id").asInstanceOf[String]
        ref.tpe = 1 // TYPE_TRANSACTIONAL
        ref.signedBy += capsuleIn.getFinalRole("creator")
        ref.signedBy += capsuleIn.getFinalRole("owner")

        capsuleOut.addReference(ref)
      })

      capsuleOut.lockData
      capsuleOut.sign(keys)
      capsuleOut.lock
    })

    capsulesIn.foreach(capsuleIn => {
      capsulesOut.foreach(capsuleOut => {
        val ref = new Reference

        ref.transactionalId = capsuleOut.getTransactional("id").asInstanceOf[String]
        ref.tpe = 1 // TYPE_TRANSACTIONAL
        ref.signedBy += capsuleOut.getFinalRole("creator")
        ref.signedBy += capsuleOut.getFinalRole("owner")

        capsuleIn.addReference(ref)
      })
      capsuleIn.lockData
      capsuleIn.lock
    })
  }

  def signStep2(keys: Seq[PrivateKey]): Unit = {
    val hashIndex = HashMap[String, HashId]()

    capsulesOut.foreach(capsuleOut => {
      val transactionId = capsuleOut.getTransactional("id").asInstanceOf[String]

      capsuleOut.sign(keys)
      capsuleOut.lock

      hashIndex(transactionId) = capsuleOut.currentHashId
    })

    capsulesIn.foreach(capsuleIn => {
      val references = capsuleIn.getTransactional("references").asInstanceOf[ListBuffer[Reference]]

      references.foreach(ref => ref.contractId = hashIndex(ref.transactionalId))
      capsuleIn.setTransactional("references", references)

      capsuleIn.lockData
      capsuleIn.sign(keys)
      capsuleIn.lock
    })
  }

  def signStep3(keys: Seq[PrivateKey]): Unit = {
    capsulesIn.foreach(capsuleIn => {
      capsuleIn.sign(keys)
      capsuleIn.lock
    })
  }

  def getTransactionPack(key: PrivateKey): TransactionPack = {
    val capsule = Capsule(key.publicKey)

    def addNew(capsules: ListBuffer[Capsule]): Unit =
      capsule.addNewCapsules(capsules)

    addNew(capsulesIn)
    addNew(capsulesOut)
    // addNew(capsulesExtra)

    capsule.setTTL(3, "month")
    capsule.lockData
    capsule.sign(key)
    capsule.lock

    val references = capsulesIn.map(_.toBOSS)
    references ++= capsulesOut.map(_.toBOSS)
    references ++= capsulesExtra.map(_.toBOSS)
    references ++= capsulesRevoking.map(_.toBOSS)

    val tpack = new TransactionPack(capsule.currentBinary)
    tpack.setSubItems(references)
    tpack
  }
}

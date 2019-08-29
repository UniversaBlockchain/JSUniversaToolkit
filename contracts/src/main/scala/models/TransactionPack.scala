package models

import boss._
import boss.jsany._
import models.contracts._

import tools.universa.UniversaTools._

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{ JSExportStatic, JSExportTopLevel }

case class TransactionPackJS(
  contract: Seq[Byte],
  subItems: ListBuffer[Seq[Byte]] = ListBuffer[Seq[Byte]](),
  referencedItems: ListBuffer[Seq[Byte]] = ListBuffer[Seq[Byte]]()
) extends BossCase

@JSExportTopLevel("Universa.TransactionPack")
class TransactionPackExported(val tp: TransactionPack) extends js.Object {
  @deprecated("should be removed as not clear")
  def contract: js.Array[Byte] = tp.contract.toJSArray

  def toBOSS(): js.Array[Byte] = tp.toBOSS.toJSArray

  def sign(privateKey: PrivateKeyExported): Unit =
    tp.sign(privateKey.privateKey)

  def hasSignature(address: String): Boolean =
    tp.hasSignature(decode58(address))

  def getSubItem(hashId64: String): js.UndefOr[CapsuleExported] = {
    val hashId = HashId.fromBytes(decode64(hashId64))
    tp.getSubItem(hashId).map(new CapsuleExported(_)).orUndefined
  }

  def getReferencedItem(hashId64: String): js.UndefOr[CapsuleExported] = {
    val hashId = HashId.fromBytes(decode64(hashId64))
    tp.getReferencedItem(hashId).map(new CapsuleExported(_)).orUndefined
  }

  def getReferencedItems(): js.Array[CapsuleExported] =
    tp.getReferencedItems.map(new CapsuleExported(_)).toJSArray

  def getNewItems(): js.Array[CapsuleExported] =
    tp.getNewItems.map(new CapsuleExported(_)).toJSArray

  def getRevokingItems(): js.Array[CapsuleExported] =
    tp.getRevokingItems.map(new CapsuleExported(_)).toJSArray

  def getMainCapsule(): CapsuleExported =
    new CapsuleExported(tp.mainCapsule)
}

class TransactionPack(var contract: Seq[Byte]) extends BossSerializable {
  var subItems = ListBuffer.empty[Seq[Byte]]
  var referencedItems = ListBuffer.empty[Seq[Byte]]

  def mainCapsule: Capsule = new Capsule(contract)

  private def getReferenced: ListBuffer[Capsule] =
    referencedItems.map(new Capsule(_))

  private def getSub: ListBuffer[Capsule] =
    subItems.map(new Capsule(_))

  def toBOSS: Seq[Byte] = Boss.dump(this)

  def toJS: TransactionPackJS =
    TransactionPackJS(contract, subItems, referencedItems)

  def setSubItems(items: ListBuffer[Seq[Byte]]): Unit =
    subItems = items

  def setSubItems(item: Seq[Byte]): Unit =
    setSubItems(ListBuffer(item))

  def addSubItem(item: Seq[Byte]): Unit =
    subItems += item

  def addSubItem(item: Capsule): Unit =
    subItems += item.currentBinary

  def setReferencedItems(items: ListBuffer[Seq[Byte]]): Unit =
    referencedItems = items

  def setReferencedItems(item: Seq[Byte]): Unit =
    setReferencedItems(ListBuffer(item))

  def addReferencedItem(item: Seq[Byte]): Unit =
    referencedItems += item

  def addReferencedItem(capsule: Capsule): Unit =
    referencedItems += capsule.currentBinary

  def this(serialized: TransactionPackJS) = {
    this(serialized.contract)

    if (serialized.subItems != null)
      subItems = serialized.subItems

    if (serialized.referencedItems != null)
      referencedItems = serialized.referencedItems
  }

  def this(capsule: Capsule) = this(capsule.currentBinary)

  def getSubItem(id: HashId): Option[Capsule] =
    getSub.find(_.hashId.equals(id))

  def getReferencedItem(id: HashId): Option[Capsule] =
    getReferenced.find(_.hashId.equals(id))

  def getNewItems: ListBuffer[Capsule] =
    TransactionPack.capsulesByIds(getSub, mainCapsule.newIds)

  def getRevokingItems: ListBuffer[Capsule] =
    TransactionPack.capsulesByIds(getSub, mainCapsule.revokingIds)

  def getReferencedItems: ListBuffer[Capsule] = getReferenced

  def sign(privateKey: PrivateKey): Unit = {
    var capsule = mainCapsule
    capsule.sign(privateKey)
    capsule.lock
    contract = capsule.currentBinary
  }

  def hasSignature(address: Seq[Byte]): Boolean =
    mainCapsule.getPublicKeys.exists(k => k.isMatchingAddress(address))
}

object TransactionPack {
  def apply(
    capsule: Capsule,
    subItems: ListBuffer[Capsule] = ListBuffer.empty[Capsule],
    referencedItems: ListBuffer[Capsule] = ListBuffer.empty[Capsule]
  ): TransactionPack = {
    val tpack = new TransactionPack(capsule.currentBinary)
    tpack.setSubItems(subItems.map(_.currentBinary))
    tpack.setReferencedItems(referencedItems.map(_.currentBinary))
    tpack
  }

  def capsulesByIds(
    capsules: ListBuffer[Capsule],
    ids: ListBuffer[HashId]
  ): ListBuffer[Capsule] =
    capsules.filter(cap => ids.exists(id => cap.hashId.equals(id)))
}

object TransactionPackExported {
  def fromJS(serialized: BossCase): BossSerializable = {
    new TransactionPack(serialized.asInstanceOf[TransactionPackJS])
  }

  @JSExportStatic("fromBOSS")
  def fromBOSS(packed: js.Array[Byte]): TransactionPackExported = {
    val tpack = Boss.load(packed.toSeq).asInstanceOf[TransactionPack]
    new TransactionPackExported(tpack)
  }
}

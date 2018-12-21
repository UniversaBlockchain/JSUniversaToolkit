package models

import boss._
import boss.jsany._
import models.contracts._

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

case class TransactionPackJS(
  contract: Seq[Byte],
  subItems: ListBuffer[Seq[Byte]] = ListBuffer[Seq[Byte]]()
) extends BossCase

@JSExportTopLevel("Universa.TransactionPack")
class TransactionPackExported(val transactionPack: TransactionPack) extends js.Object {
  val contract = transactionPack.contract.toJSArray

  def subItems: js.Array[Seq[Byte]] = transactionPack.subItems.toJSArray

  def toBOSS(): js.Array[Byte] = transactionPack.toBOSS.toJSArray

  def getMainContract() = transactionPack.mainContract.toExport()

  def getNewContracts(): js.Array[_] = {
    transactionPack
      .getNewContracts()
      .map(_.toExport())
      .toJSArray
  }

  def getRevokingContracts(): js.Array[_] = {
    transactionPack
      .getRevokingContracts()
      .map(_.toExport())
      .toJSArray
  }
}

class TransactionPack(var contract: Seq[Byte]) extends BossSerializable {
  var subItems = ListBuffer.empty[Seq[Byte]]

  def toBOSS: Seq[Byte] = Boss.dump(this)

  def toJS: TransactionPackJS =
    TransactionPackJS(contract, subItems)

  def setItems(items: ListBuffer[Seq[Byte]]): Unit =
    subItems = items

  def addItem(item: Seq[Byte]): Unit = {
    subItems += item
  }

  def setContracts(contracts: ListBuffer[Contract]): Unit = {
    subItems = ListBuffer.empty[Seq[Byte]]

    contracts.foreach(contract =>
      subItems += contract.original.currentBinary
    )
  }

  def setCapsules(items: ListBuffer[Capsule]): Unit = {
    subItems = ListBuffer.empty[Seq[Byte]]

    items.foreach(capsule =>
      subItems += capsule.currentBinary
    )
  }

  def this(serialized: TransactionPackJS) = {
    this(serialized.contract)
    subItems = serialized.subItems
  }

  private val mainCapsule = new Capsule(contract)

  val mainContract = ContractFactory.buildFromCapsule(mainCapsule)

  def getNewContracts[T >: Contract](): ListBuffer[T] = {
    findSubItemsForHashIds(mainCapsule.newIds)
  }

  def getRevokingContracts[T >: Contract](): ListBuffer[T] = {
    findSubItemsForHashIds(mainCapsule.revokingIds)
  }

  private def findSubItemsForHashIds[T >: Contract](newIds: ListBuffer[HashId]): ListBuffer[T] = {
    val newHashIds = newIds.map(_.composite3)
    val subCapsules = subItems.map(binary => new Capsule(binary))
    val commonHashes = subCapsules.map(_.hashId.composite3).intersect(newHashIds)
    subCapsules
      .filter(sc => commonHashes.contains(sc.hashId.composite3))
      .map(sc => ContractFactory.buildFromCapsule(sc))
  }
}

object TransactionPack {
  def apply(capsule: Capsule, subItems: ListBuffer[Capsule] = ListBuffer.empty[Capsule]): TransactionPack = {
    val tpack = new TransactionPack(capsule.currentBinary)
    tpack.setCapsules(subItems)
    tpack
  }
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

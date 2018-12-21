package models.contracts

import models.{CapsuleExported, Contract, ContractExported, PublicKeyExported, SplitJoinPermission, ChangeOwnerPermission}

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.util.Try


@JSExportTopLevel("Universa.MintableContract")
class MintableContractExported(override val contract: MintableContract) extends ContractExported(contract) {
  override def templateName = ContractType.MINTABLE_CONTRACT.toString

  def defaultName: String = contract.defaultName

  def createCopy(amount: String): MintableContractExported = {
    new MintableContractExported(contract.createCopy(amount))
  }
}

class MintableContract(originalPacked: Seq[Byte],
                       tempPacked: Option[Seq[Byte]] = None,
                       pendingPacked: Option[Seq[Byte]] = None)
  extends Contract(originalPacked, tempPacked, pendingPacked) with ContractTemplate {

  override def toExport[T >: ContractExported](): T = new MintableContractExported(this)

  override def defaultName: String = {
    Try {
      val amount = original.getAmount.bigDecimal.toPlainString
      val shortName = original.contract.getDefinitionData("short_currency")
      s"$amount $shortName"
    }.getOrElse("")
  }

  def createCopy(amount: String): MintableContract = {
    val capsuleCopy = original.copyAsRoot
    capsuleCopy.lockDataAndSign()
    new MintableContract(capsuleCopy.currentBinary)
  }
}

object MintableContractExported {
  @JSExportStatic("create")
  def create(
    key: PublicKeyExported,
    amount: String,
    minUnit: Float,
    unitFull: String,
    unitShort: String,
    description: String,
    useLongAddress: Boolean = false
  ): MintableContractExported = {
    val cap = CapsuleExported.createByKey(key, useLongAddress)
    cap.contract.setDefinitionData("template_name", ContractType.MINTABLE_CONTRACT.toString)

    cap.contract.setDefinitionData("name", unitFull)
    cap.contract.setDefinitionData("currency", unitShort)
    cap.contract.setDefinitionData("short_currency", unitShort)
    cap.contract.setDefinitionData("description", description)

    val splitJoin = SplitJoinPermission(
      "owner",
      minUnit.toString,
      minUnit.toString,
      "amount",
      ListBuffer("definition.data.currency", "definition.issuer")
    )
    cap.contract.addPermission(splitJoin)
    cap.contract.addPermission(ChangeOwnerPermission("owner"))
    //this must come after permissions as permission-minUnit make influence on amount!
    cap.setAmount(BigDecimal(amount))

    cap.lockDataAndSign()

    val contract = new MintableContract(cap.currentBinary)
    new MintableContractExported(contract)
  }
}

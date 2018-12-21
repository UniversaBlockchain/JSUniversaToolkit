package models.contracts

import models.{CapsuleExported, Contract, ContractExported, PublicKeyExported}

import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.util.Try

@JSExportTopLevel("Universa.UnitContract")
class UnitContractExported(override val contract: UnitContract) extends ContractExported(contract) {
  override def templateName = ContractType.UNIT_CONTRACT.toString

}

class UnitContract(originalPacked: Seq[Byte],
                   tempPacked: Option[Seq[Byte]] = None,
                   pendingPacked: Option[Seq[Byte]] = None)
  extends Contract(originalPacked, tempPacked, pendingPacked) with ContractTemplate {

  override def defaultName: String = {
    Try {
      val amount = original.getAmount.bigDecimal.toPlainString
      val shortName = original.contract.getDefinitionData("unit_short_name")
      s"$amount $shortName"
    }.getOrElse("")
  }

  override def toExport[T >: ContractExported](): T = new UnitContractExported(this)
}

object UnitContractExported {
  @JSExportStatic
  def create(key: PublicKeyExported, useLongAddress: Boolean): UnitContractExported = {
    val cap = CapsuleExported.createByKey(key, useLongAddress)
    cap.contract.setDefinitionData("template_name", ContractType.UNIT_CONTRACT.toString)
    val contract = new UnitContract(cap.currentBinary)
    new UnitContractExported(contract)
  }
}
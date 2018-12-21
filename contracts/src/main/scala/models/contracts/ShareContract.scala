package models.contracts

import models.{CapsuleExported, Contract, ContractExported, PublicKeyExported}

import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

@JSExportTopLevel("Universa.ShareContract")
class ShareContractExported(override val contract: ShareContract) extends ContractExported(contract) {
  override def templateName = ContractType.SHARE_CONTRACT.toString
}

class ShareContract(originalPacked: Seq[Byte],
                    tempPacked: Option[Seq[Byte]] = None,
                    pendingPacked: Option[Seq[Byte]] = None)
  extends Contract(originalPacked, tempPacked, pendingPacked) with ContractTemplate {

  override def toExport[T >: ContractExported](): T = new ShareContractExported(this)

  def calculate(): Unit = {
    val amount: BigDecimal = this.temp.getAmount
    this.temp.setAmount(amount)
  }
}

object ShareContractExported {
  @JSExportStatic
  def create(key: PublicKeyExported, useLongAddress: Boolean): ShareContractExported = {
    val cap = CapsuleExported.createByKey(key, useLongAddress)
    cap.contract.setDefinitionData("template_name", ContractType.SHARE_CONTRACT.toString)
    val contract = new ShareContract(cap.currentBinary)
    new ShareContractExported(contract)
  }
}
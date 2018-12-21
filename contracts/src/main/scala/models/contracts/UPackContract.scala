package models.contracts

import models._
import models.errors.BalanceError

import scala.scalajs.js.annotation.JSExportTopLevel


@JSExportTopLevel("Universa.UPackContract")
class UPackContractExported(override val contract: UPackContract) extends ContractExported(contract) {

  override def templateName: String = ContractType.UPACK_CONTRACT.toString
  def getBalance(testMode: Boolean = false): Int = contract.getBalance(testMode)
}


class UPackContract(originalPacked: Seq[Byte],
                    tempPacked: Option[Seq[Byte]] = None,
                    pendingPacked: Option[Seq[Byte]] = None)
  extends Contract(originalPacked, tempPacked, pendingPacked) with ContractTemplate {

  private val testKey = "test_transaction_units"
  private val mainKey = "transaction_units"

  override def toExport[T >: ContractExported](): T = new UPackContractExported(this)

  def getBalance(testMode: Boolean = false): Int = {
    val amountKey = getAmountKey(testMode)
    temp.getState(s"data.$amountKey").asInstanceOf[Int]
  }

  private def getAmountKey(testMode: Boolean = false): String = {
    if (testMode) testKey
    else mainKey
  }

  def createPayment(amount: Int, testMode: Boolean = false): Unit = synchronized {
    def setBalance(amount: Int, testMode: Boolean = false) = {
      val amountKey = getAmountKey(testMode)
      temp.setState(s"data.$amountKey", amount)
    }

    val balance = getBalance(testMode)
    if (balance - amount < 0) throw new BalanceError("Not enough U to pay")
    createRevision
    temp.setCreatorLink("owner")
    setBalance(balance - amount, testMode)
  }

}

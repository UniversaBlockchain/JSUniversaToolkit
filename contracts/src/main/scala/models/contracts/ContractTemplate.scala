package models.contracts

trait ContractTemplate

object ContractType extends Enumeration {
  type ContractType = Value
  val GENERAL_CONTRACT = Value("GENERAL_CONTRACT")
  val NOTARY_CONTRACT = Value("NOTARY_CONTRACT")
  val SHARE_CONTRACT = Value("SHARE_CONTRACT")
  val UNIT_CONTRACT = Value("UNIT_CONTRACT")
  val UPACK_CONTRACT = Value("transaction units pack")
  val MINTABLE_CONTRACT = Value("MINTABLE_UNIT_CONTRACT")
}

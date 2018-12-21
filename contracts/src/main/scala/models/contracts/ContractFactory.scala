package models.contracts

import models.{Capsule, Contract, RoleSimple}
import tools.universa.UniversaTools.encode58

import scala.util.Try

object ContractFactory {

  def isUPack(temp: Capsule): Boolean = {
    val templateNameOpt = Try {
      temp.getDefinition("data.name").asInstanceOf[String]
    }.toOption
    if (templateNameOpt.fold(false)(_ != "transaction units pack")) return false

    val tuLong = "J3uaVvHE7JqhvVb1c26RyDhfJw9eP2KR1KRhm2VdmYx7NwHpzdHTyEPjcmKpgkJAtzWLSPUw"
    val tuShort = "ZNuBikFEZbw71QQAFkNQtjfkmxFAdMgveTVPMGrFwo9vQwwPVE"

    val issuer = temp.getDefinition("issuer").asInstanceOf[RoleSimple]
    val issuerAddressesShort = issuer.getRawAddresses(false).map { a => encode58(a) }
    val issuerAddressesLong = issuer.getRawAddresses(true).map { a => encode58(a) }

    if (issuerAddressesShort.size != 1) return false
    if (issuerAddressesLong.size != 1) return false

    val addressShort = issuerAddressesShort.head
    val addressLong = issuerAddressesLong.head

    addressLong == tuLong || addressShort == tuShort
  }


  def getContractType(cap: Capsule): ContractType.Value = {
    if (isUPack(cap))
      ContractType.UPACK_CONTRACT
    else {
      Try {
        val tname = cap.contract.getDefinitionData("template_name").toString
        if (tname == "NOTARY") ContractType.NOTARY_CONTRACT
        else ContractType.withName(tname)
      }.getOrElse {
        if (cap.getSplitJoin.isDefined) ContractType.UNIT_CONTRACT
        else ContractType.GENERAL_CONTRACT
      }
    }
  }

  private def getContractByType[T >: Contract](contractTypeEnum: ContractType.Value,
                                               originalPacked: Seq[Byte],
                                               tempPacked: Option[Seq[Byte]],
                                               pendingPacked: Option[Seq[Byte]]): T = {
    contractTypeEnum match {
      case ContractType.UPACK_CONTRACT =>
        new UPackContract(originalPacked, tempPacked, pendingPacked)
      case ContractType.UNIT_CONTRACT =>
        new UnitContract(originalPacked, tempPacked, pendingPacked)
      case ContractType.MINTABLE_CONTRACT =>
        new MintableContract(originalPacked, tempPacked, pendingPacked)
      case ContractType.SHARE_CONTRACT =>
        new ShareContract(originalPacked, tempPacked, pendingPacked)
      case ContractType.NOTARY_CONTRACT =>
        new NotaryContract(originalPacked, tempPacked, pendingPacked)
      case _ =>
        new Contract(originalPacked, tempPacked, pendingPacked)
    }
  }

  def buildFromCapsule[T >: Contract](cap: Capsule): T = {
    val cTypeOpt = getContractType(cap)
    getContractByType(cTypeOpt, cap.currentBinary, None, None)
  }

  def buildFromBytes[T >: Contract](
                                     originalPacked: Seq[Byte],
                                     tempPacked: Option[Seq[Byte]],
                                     pendingPacked: Option[Seq[Byte]]
                                   ): T = {
    val temp = new Capsule(tempPacked.getOrElse(originalPacked))
    val cType = getContractType(temp)
    getContractByType(cType, originalPacked, tempPacked, pendingPacked)
  }

}

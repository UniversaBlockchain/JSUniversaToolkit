package universa

import boss.Boss
import models._
import xchange.{LocalTimeDifference, LocalTimeDifferenceJS, OrderExported, OrderJS}
// import cloud.models._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object CryptoCloudStarter {

  def main(args: Array[String]): Unit = {
    if (!js.isUndefined(dom.window)) {

      dom.window.onload = { _ =>
        registerBasicModels()
      }
    }
  }

  @JSExportTopLevel("Universa.registerBasicModels")
  def registerBasicModels(): Unit = {
    Boss.registerClass[PublicKeyJS](PublicKeyExported.fromJS, "RSAPublicKey")
    Boss.registerClass[KeyRecordJS](KeyRecord.fromJS, "KeyRecord")
    Boss.registerClass[KeyAddressJS](KeyAddress.fromJS, "KeyAddress")

    Boss.registerClass[StringOrDoubleJS](StringOrDouble.fromJS, "StringOrDouble")
    Boss.registerClass[PairJS](PairExported.fromJS, "Pair")

    Boss.registerClass[RoleSimpleJS](RoleSimple.fromJS, "SimpleRole")
    Boss.registerClass[RoleLinkJS](RoleLink.fromJS, "RoleLink")
    Boss.registerClass[RoleListJS](RoleList.fromJS, "RoleList")

    Boss.registerClass[RevokePermissionJS](RevokePermission.fromJS, "RevokePermission")
    Boss.registerClass[ChangeOwnerPermissionJS](ChangeOwnerPermission.fromJS, "ChangeOwnerPermission")
    Boss.registerClass[ChangeNumberPermissionJS](ChangeNumberPermission.fromJS, "ChangeNumberPermission")
    Boss.registerClass[SplitJoinPermissionJS](SplitJoinPermission.fromJS, "SplitJoinPermission")
    Boss.registerClass[ModifyDataPermissionJS](ModifyDataPermission.fromJS, "ModifyDataPermission")

    Boss.registerClass[UniversaContractJS](UniversaContract.fromJS, "UniversaContract")
    Boss.registerClass[HashIdJS](HashId.fromJS, "HashId")
    Boss.registerClass[TransactionPackJS](TransactionPackExported.fromJS, "TransactionPack")

    Boss.registerClass[ContractBinaryJS](ContractBossConverter.contractBinaryFromJS, "ContractBinary")
    Boss.registerClass[ContractIdsJS](ContractBossConverter.ContractIdsFromJS, "ContractIds")
    Boss.registerClass[ParcelJS](ParcelExported.fromJS, "Parcel")
    Boss.registerClass[OrderJS](OrderExported.fromJS, "Order")

    // Boss.registerClass[FileBossJS](FileBoss.fromJS, "File")
    Boss.registerClass[LocalTimeDifferenceJS](LocalTimeDifference.fromJS, "LocalTimeDifference")
  }
}

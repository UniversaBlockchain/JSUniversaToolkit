package universa

import boss.Boss
import files.{ FileBoss, FileBossJS }
import models._
import org.scalajs.dom
import storage.Storage

import xchange.{LocalTimeDifference, LocalTimeDifferenceJS, OrderExported, OrderJS}

import scala.async.Async.async
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object main {
  def main(args: Array[String]): Unit = {
    if (!js.isUndefined(dom.window)) {
      dom.window.onload = { _ =>
        async {
          registerModels()
        }
      }
    }
  }

  @JSExportTopLevel("Universa.registerModels")
  def registerModels(): Unit = {
    Boss.registerClass[PublicKeyJS](PublicKeyExported.fromJS, "RSAPublicKey")
    Boss.registerClass[KeyRecordJS](KeyRecord.fromJS, "KeyRecord")
    Boss.registerClass[KeyAddressJS](KeyAddress.fromJS, "KeyAddress")

    Boss.registerClass[StringOrDoubleJS](StringOrDouble.fromJS, "StringOrDouble")
    Boss.registerClass[PairJS](PairExported.fromJS, "Pair")

    Boss.registerClass[RoleSimpleJS](RoleSimple.fromJS, "SimpleRole")
    Boss.registerClass[RoleLinkJS](RoleLink.fromJS, "RoleLink")
    Boss.registerClass[RoleListJS](RoleList.fromJS, "ListRole")

    Boss.registerClass[RevokePermissionJS](RevokePermission.fromJS, "RevokePermission")
    Boss.registerClass[ChangeOwnerPermissionJS](ChangeOwnerPermission.fromJS, "ChangeOwnerPermission")
    Boss.registerClass[ChangeNumberPermissionJS](ChangeNumberPermission.fromJS, "ChangeNumberPermission")
    Boss.registerClass[SplitJoinPermissionJS](SplitJoinPermission.fromJS, "SplitJoinPermission")
    Boss.registerClass[ModifyDataPermissionJS](ModifyDataPermission.fromJS, "ModifyDataPermission")

    Boss.registerClass[ReferenceJS](Reference.fromJS, "com.icodici.universa.contract.Reference")
    Boss.registerClass[UniversaContractJS](UniversaContract.fromJS, "UniversaContract")
    Boss.registerClass[HashIdJS](HashId.fromJS, "HashId")
    Boss.registerClass[TransactionPackJS](TransactionPackExported.fromJS, "TransactionPack")

    Boss.registerClass[ContractBinaryJS](ContractBossConverter.contractBinaryFromJS, "ContractBinary")
    Boss.registerClass[ContractIdsJS](ContractBossConverter.ContractIdsFromJS, "ContractIds")
    Boss.registerClass[ParcelJS](ParcelExported.fromJS, "Parcel")
    Boss.registerClass[OrderJS](OrderExported.fromJS, "Order")

    Boss.registerClass[FileBossJS](FileBoss.fromJS, "File")
    Boss.registerClass[LocalTimeDifferenceJS](LocalTimeDifference.fromJS, "LocalTimeDifference")

    Boss.registerClass[BulkExchangeJS](BulkExchange.fromJS, "BulkExchange")

    xchange.XChangeAPI.getTimeOffsetWithUniversaNetwork()
      .foreach{offset =>
        val diffObject = new LocalTimeDifference(offset, js.Date.now())
        Storage.timeStorage.set("universaTimeOffset", diffObject)
      }
  }
}

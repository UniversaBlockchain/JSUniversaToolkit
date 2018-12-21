package models

import boss._
import cloud.{Api, Item}
import models.contracts.{ContractType, _}
import models.errors.NoKeyError
import node.{NodeApi, NodeApiExported}
import storage.Storage._
import tools.universa.ImplicitConverters._
import tools.universa.SHA
import tools.universa.UniversaTools._
import xchange.XChangeAPI

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.typedarray.Int8Array

@JSExportTopLevel("Universa.Contract")
class ContractExported(val contract: Contract) extends js.Object {
  type Dict = HashMap[String, Any]
  type JSDict = js.Dictionary[Any]

  def original: CapsuleExported = new CapsuleExported(contract.original)

  def temp: CapsuleExported = new CapsuleExported(contract.temp)

  def pending: CapsuleExported = {
    contract.pending.map(pending => new CapsuleExported(pending)).orNull
  }

  def contractName: String = contract.contractName.orNull

  def templateName: String = ContractType.GENERAL_CONTRACT.toString

  def walletID(useLongAddress: Boolean = false): String = contract.walletID(useLongAddress)

  def setOwner(param: js.Object, useLongAddress: Boolean): Unit = {
    param match {
      case paramKey: PublicKeyExported =>
        val owner = new RoleSimple("owner", paramKey.publicKey, useLongAddress)
        temp.capsule.setState("owner", owner)
      case _ =>
        val keyAddress = new KeyAddress(param.asInstanceOf[Int8Array].toJSArray)
        val owner = new RoleSimple("owner", keyAddress)
        temp.capsule.setState("owner", owner)
    }
  }

  @deprecated("use method with 2 arguments instead")
  def setOwner(param: js.Object): Unit = {
    if (param.isInstanceOf[PublicKeyExported]) {
      val owner = new RoleSimple("owner", param.asInstanceOf[PublicKeyExported].publicKey)
      temp.capsule.setState("owner", owner)
    } else {
      val keyAddress = new KeyAddress(param.asInstanceOf[Int8Array].toJSArray)
      val owner = new RoleSimple("owner", keyAddress)
      temp.capsule.setState("owner", owner)
    }
  }

  def createRevision(): Unit = contract.createRevision

  def isUPack: Boolean = contract.isUPack

  def checkPending(api: NodeApiExported): js.Promise[JSDict] =
    contract.checkPending(api.nodeApi)

  def checkOriginal(api: NodeApiExported): js.Promise[JSDict] =
    contract.checkOriginal(api.nodeApi)

  def checkTemp(api: NodeApiExported): js.Promise[JSDict] =
    contract.checkTemp(api.nodeApi)

  def setState(path: String, value: Any): Unit =
    temp.capsule.setState(path, value)

  def setDefinition(path: String, value: Any): Unit =
    temp.capsule.setDefinition(path, value)

  def setContractName(newName: String): Unit =
    contract.contractName = Some(newName)

  def getTransactionPack(): TransactionPackExported =
    new TransactionPackExported(contract.getTransactionPack)

  def updateStorage(): ContractIds = contract.saveOrUpdateContractIds()

  def updateOriginal(): Unit = contract.updateOriginal

  def revertOriginal(): Unit = contract.revertOriginal

  def toJsObject(): js.Dictionary[_] = {
    HashMap(
      "original" -> contract.original.currentBinary.toJSArray,
      "temp" -> contract.temp.currentBinary.toJSArray,
      "pending" -> contract.pending.map(_.currentBinary.toJSArray).orNull,
      "state" -> contract.state,
      "contractName" -> contract.contractName.orNull,
      "filesCloudId" -> contract.filesCloudId.getOrElse(null),
      "cloudId" -> contract.contractCloudId.getOrElse(null),
      "mainSiblingId" -> contract.mainSiblingId.map(_.composite3.toJSArray).orNull
    ).toJSDictionary
  }

  private def contractSummary(): mutable.HashMap[String, Any] = {
    val map = HashMap[String, Any](
      "original" -> contract.original.currentBinary,
      "temp" -> contract.temp.toBOSS,
      "state" -> contract.state
    )
    contract.pending.foreach{p => map.put("pending", p.currentBinary)}
    contract.contractName.foreach(n => map.put("contractName", n))
    contract.filesCloudId.foreach(c => map.put("filesCloudId", c))
    contract.contractCloudId.foreach(c => map.put("cloudId", c))
    contract.mainSiblingId.foreach(n => map.put("mainSiblingId", n))
    map
  }

  def toCloud(api: Api): js.Promise[Double] = {
    val itemF = contract.contractCloudId.map { cloudId =>
      api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    }.getOrElse {
      var item: Item = null
      item = new Item(api)
      item.setTag(ContractExported.ItemTag)
      Future.successful(item)
    }
    itemF.flatMap { item =>
      item.setPrivateData("instance", contractSummary())
      item.setPrivateData("version", 3)
      item.save.map { _ =>
        val itemId = item.id.get.toDouble
        contract.contractCloudId = Option(itemId)
        itemId
      }
    }.toJSPromise
  }

  def deleteFromCloud(api: Api): js.Promise[Boolean] = {
    val cloudId = contract.contractCloudId.getOrElse(throw new RuntimeException("Not known cloud id"))
    val loaded = api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    loaded.flatMap(_.destroy)
  }

  def createParcel(
                    upack: UPackContractExported,
                    cost: Int,
                    keys: js.Array[PrivateKeyExported],
                    testMode: Boolean = false,
                    useLongAddress: Boolean = false
                  ): ParcelExported =
    contract.createParcel(upack.contract, cost, keys.map(_.privateKey), testMode, useLongAddress)
}

class Contract(
  originalPacked: Seq[Byte],
  tempPacked: Option[Seq[Byte]] = None,
  pendingPacked: Option[Seq[Byte]] = None
) extends ContractTemplate {

  type Dict = HashMap[String, Any]
  type JSDict = js.Dictionary[Any]

  val uuid: String = encode64(originalPacked)

  var original: Capsule = new Capsule(originalPacked)
  var temp: Capsule = new Capsule(tempPacked.getOrElse(originalPacked))
  var pending: Option[Capsule] = pendingPacked.map(p => new Capsule(p))

  var state = "DRAFT"
  var contractName: Option[String] = Option(defaultName)
  var filesCloudId: Option[Double] = None
  var contractCloudId: Option[Double] = None
  var mainSiblingId: Option[HashId] = None
  var errors: ListBuffer[String] = ListBuffer.empty[String]

  def toExport[T >: ContractExported](): T = new ContractExported(this)

  def defaultName: String = null

  def registerFree(api: NodeApi): Future[HashMap[String, Any]] = {
    pending = Some(new Capsule(temp.currentBinary))
    api.registerContract(temp.currentBinary) map { response =>
      state = get(response, "itemResult.state").asInstanceOf[String]
      response
    }
  }

  def updateOriginal: Unit = {
    pending match {
      case Some(capsule) => {
        original = new Capsule(capsule.currentBinary)
        pending = None
      }

      case None => {}
    }
  }

  def revertOriginal: Unit = {
    temp = new Capsule(original.currentBinary)
    pending = None
  }

  def getTransactionPack: TransactionPack = {
    if (original.isParentFor(temp))
      TransactionPack(temp, ListBuffer(original))
    else
      TransactionPack(original)
  }

  def createParcel(
                    upack: UPackContract,
                    cost: Int,
                    keys: mutable.Seq[PrivateKey],
                    testMode: Boolean = false,
                    useLongAddress: Boolean = false
                  ): ParcelExported = {
    if (cost < 1) throw new RuntimeException("cost can't be less than 1 U")

    upack.createPayment(cost, testMode)
    val paymentSignerOpt = upack.original.getKeyForRole("owner", keys, useLongAddress)
    // val contractSignerOpt = temp.getKeyForRole("creator", keys, useLongAddress)

    val paymentSigner = paymentSignerOpt.getOrElse(
      throw new NoKeyError("There's no key for payment owner")
    )
    // val contractSigner = contractSignerOpt.getOrElse(
    //   throw new NoKeyError("There's no key for contract creator")
    // )

    upack.temp.lockDataAndSign(paymentSigner)

    //    println("PACKED UPACK")
    //    println(encode64(upack.temp.currentBinary))

    // temp.lockDataAndSign(contractSigner)

    val payloadBinary = temp.currentBinary
    val paymentBinary = upack.temp.currentBinary

    upack.pending = Some(new Capsule(paymentBinary))
    pending = Some(new Capsule(payloadBinary))

    //    println("PACKED CONTRACT")
    //    println(encode64(temp.currentBinary))
    //    val con2 = new UPackContract(upack.temp.currentBinary)
    //    println(con2.getBalance(true))

    val paymentPack = new TransactionPack(paymentBinary)
    paymentPack.setItems(ListBuffer(upack.original.currentBinary))

    val payloadPack = new TransactionPack(payloadBinary)
    if (temp.revision > original.revision) {
      payloadPack.setItems(ListBuffer(original.currentBinary))
    }

    ParcelExported(payloadPack, paymentPack)
  }

  def checkPending(api: NodeApi): Future[Dict] = {
    pending match {
      case Some(cap) => api.checkContract(cap.currentHashId)
      case None => throw new RuntimeException("No pending version found")
    }
  }

  def checkOriginal(api: NodeApi): Future[Dict] = {
    api.checkContract(original.currentHashId)
  }

  def checkTemp(api: NodeApi): Future[Dict] = {
    api.checkContract(temp.currentHashId)
  }

  def createRevision: Unit = {
    val originalId = original.currentHashId

    temp.setRevision(temp.revision + 1)
    temp.setParent(originalId)
    temp.newIds = ListBuffer[HashId]()
    temp.revokingIds = ListBuffer(originalId)
    temp.setState("created_at", XChangeAPI.getNetworkJsDate())

    if (temp.getOrigin.isEmpty) temp.setOrigin(originalId)
  }

  def isUPack: Boolean = ContractFactory.isUPack(temp)

  def saveOrUpdateContractIds(): ContractIds = {
    val contractIdsToSave = contractIdsStorage.find(_.uuid == uuid)
      .map(existingContractIds =>
        existingContractIds.copy(
          name = contractName.orNull,
          original = original.uuid,
          draft = temp.uuid,
          pending = pending.map(_.uuid).orNull,
          filesCloudId = filesCloudId.map(_.toString).orNull))
      .getOrElse(
        ContractIds(uuid,
          contractName.orNull,
          original.uuid,
          temp.uuid,
          pending.map(_.uuid).orNull,
          filesCloudId.map(_.toString).orNull
        )
      )
    contractIdsStorage.set(uuid, contractIdsToSave)
    contractIdsToSave
  }

  def saveAllToStorage(): ContractIds = {
    ContractExported.saveDraftVersion(this.uuid, this.temp.originalBinary.toJSArray)
    ContractExported.saveOriginalVersion(this.uuid, this.original.originalBinary.toJSArray)
    this.pending.foreach(p => ContractExported.savePendingVersion(this.uuid, p.originalBinary.toJSArray))
    saveOrUpdateContractIds()
  }

  def walletID(useLongAddress: Boolean = false): String = {
    temp.getSplitJoin match {
      case None => ""
      case Some(_) =>
        val fieldValues =
          temp.getSplitJoin
          .view
          .flatMap(_.joinMatchFields.sorted)
          .map{field =>
            val value = temp.getField(field)

            if (field == "state.origin") {
              if (value == null) original.currentHashId.composite3
              else value.asInstanceOf[HashId].composite3
            }

            else if (field == "definition.issuer")
              value.asInstanceOf[RoleSimple].getAddress58(useLongAddress)

            else value
          }
          .force
          .mkString

        encode64(new SHA(256).get(fieldValues))
    }
  }
}

object ContractExported {
  val ItemTag: String = "contract"

  @JSExportStatic("fromBOSS")
  def fromBOSS[T >: ContractExported](packed: js.Array[Byte]): T = {
    ContractFactory.buildFromBytes(packed.toSeq, None, None).toExport()
  }

  @JSExportStatic("fromJsObject")
  def fromJSObject[T >: ContractExported](dict: js.Dictionary[_]): T = {
    fromMap(dict.toMap, false)
  }

  private def fromMap[T >: ContractExported](
    map: scala.collection.Map[String, Any],
    usingBoss: Boolean
  ): T = {
    def getCapsulePacked(key: String): Seq[Byte] = {
      if (usingBoss) readBytesSafe(map(key))
      else map(key).asInstanceOf[js.Array[Byte]].toSeq
    }

    val original = getCapsulePacked("original")
    val temp = getCapsulePacked("temp")

    val pending =
      if (usingBoss) map.get("pending").flatMap(Option(_)).map(_.asInstanceOf[Seq[Byte]])
      else map.get("pending").flatMap(Option(_)).map(_.asInstanceOf[js.Array[Byte]].toSeq)

    val state = map("state").asInstanceOf[String]
    val contractName = map.get("contractName").flatMap(Option(_)).map(_.asInstanceOf[String])
    val filesCloudId = map.get("filesCloudId").flatMap(Option(_)).map(_.asInstanceOf[Double])
    val cloudId = map.get("cloudId").flatMap(Option(_)).map(_.asInstanceOf[Double])

    val contract = ContractFactory.buildFromBytes(original, Some(temp), pending)
    contract.state = state
    contract.contractName = contractName
    contract.filesCloudId = filesCloudId
    contract.contractCloudId = cloudId
    if (usingBoss) {
      val mainSiblingId = map.get("mainSiblingId").flatMap(Option(_)).map(_.asInstanceOf[HashId])
      contract.mainSiblingId = mainSiblingId
    } else {
      val mainSiblingId = map.get("mainSiblingId")
        .flatMap(Option(_))
        .map(_.asInstanceOf[js.Array[Byte]].toSeq)
      contract.mainSiblingId = mainSiblingId.map(bytes => HashId(bytes))
    }

    contract.toExport()
  }

  @JSExportStatic
  def fromItem[T >: ContractExported](item: Item): T = {
    fromItemOpt(item).orNull
  }

  private def fromItemOpt[T >: ContractExported](item: Item): Option[T] = {
    val version = item.priv.get("version").map(_.asInstanceOf[Int]).getOrElse(1)
    version match {
      case 3 =>
        val instance = item.priv("instance")
        val map = instance.asInstanceOf[mutable.HashMap[String, Any]]
        val contractWrapper = fromMap(map, true)
        val itemId = item.id.flatMap(Option(_)).map(_.asInstanceOf[Double])
        contractWrapper.contract.contractCloudId = itemId
        Some(contractWrapper)
      case _ => None
    }
  }

  @JSExportStatic
  def fromCloud[T >: ContractExported](cloudId: Double, api: Api): js.Promise[T] = {
    val loaded = api.getItem({ api => new Item(api) }, mutable.HashMap[String, Any]("id" -> cloudId))
    loaded
      .map { fromItem }
      .toJSPromise
  }


  @JSExportStatic
  def allFromCloud(api: Api): js.Promise[js.Array[_]] = {
    cloud.Tools.importAllObjects(api, ContractExported.ItemTag, fromItemOpt)
      .toJSPromise
  }

  @JSExportStatic("createToken")
  def createToken(
                   key: PublicKeyExported,
                   amount: String,
                   minUnit: Float,
                   unitFull: String,
                   unitShort: String,
                   description: String
                 ): UnitContractExported = {
    val cap = CapsuleExported.createToken(key, amount, minUnit, unitFull, unitShort, description)
    cap.lockDataAndSign()

    val contract = new UnitContract(cap.currentBinary, Some(cap.currentBinary))
    new UnitContractExported(contract)
  }

  @JSExportStatic("createToken")
  def createToken(
                   key: PublicKeyExported,
                   amount: String,
                   minUnit: Float,
                   unitFull: String,
                   unitShort: String,
                   description: String,
                   useLongAddress: Boolean
                 ): UnitContractExported = {
    val cap = CapsuleExported.createToken(key, amount, minUnit, unitFull, unitShort, description, useLongAddress)
    cap.lockDataAndSign()

    val contract = new UnitContract(cap.currentBinary, Some(cap.currentBinary))
    new UnitContractExported(contract)
  }

  @JSExportStatic("createFixedToken")
  def createFixedToken(
                        key: PublicKeyExported,
                        amount: String,
                        minUnit: Float,
                        unitFull: String,
                        unitShort: String,
                        description: String,
                        useLongAddress: Boolean
                      ): ContractExported = {
    createToken(key, amount, minUnit, unitFull, unitShort, description, useLongAddress)
  }

  @deprecated("Use create with more parameters instead")
  @JSExportStatic("create")
  def create[T >: ContractExported](key: PublicKeyExported): T = {
    val cap = CapsuleExported.createByKey(key)
    cap.contract.setDefinitionData("template_name", "GENERAL_CONTRACT")
    cap.lockDataAndSign()

    val contract = ContractFactory.buildFromCapsule(cap)
    contract.toExport()
  }

  val OriginalUuid = "original"
  val DraftUuid = "draft"
  val PendingUuid = "pending"


  @JSExportStatic("findById")
  def findContractId(contractId: String): ContractIds = {
    contractIdsStorage.find {
      _.isIdHere(contractId)
    }.orNull
  }

  @JSExportStatic("setOriginal")
  def saveOriginalVersion(contractId: String, binary: js.Array[Byte]): Unit = {
    contractBinaryStorage.set(OriginalUuid + contractId, ContractBinary(binary))
  }

  @JSExportStatic("setDraft")
  def saveDraftVersion(contractId: String, binary: js.Array[Byte]): Unit = {
    contractBinaryStorage.set(DraftUuid + contractId, ContractBinary(binary))
  }

  @JSExportStatic("setPending")
  def savePendingVersion(contractId: String, binary: js.Array[Byte]): Unit = {
    contractBinaryStorage.set(PendingUuid + contractId, ContractBinary(binary))
  }

  @JSExportStatic("getOriginal")
  def getOriginalVersion(contractId: String): Option[js.Array[Byte]] = {
    getVersion(OriginalUuid, contractId)
  }

  @JSExportStatic("getDraft")
  def getDraftVersion(contractId: String): Option[js.Array[Byte]] = {
    getVersion(DraftUuid, contractId)
  }

  @JSExportStatic("getPending")
  def getPendingVersion(contractId: String): Option[js.Array[Byte]] = {
    getVersion(PendingUuid, contractId)
  }

  private def getVersion(uuid: String, contractId: String): Option[js.Array[Byte]] = {
    val contractBinOpt = contractBinaryStorage.get(uuid + contractId)
    contractBinOpt.map(_.binary.toJSArray)
  }
}

//binary
case class ContractBinary(binary: Seq[Byte]) extends BossSerializable {
  override def toJS: ContractBinaryJS = ContractBinaryJS(binary)
}

case class ContractBinaryJS(binary: Seq[Byte]) extends BossCase

//ids
@JSExportTopLevel("Universa.ContractIds")
case class ContractIds(
  uuid: String,
  name: String,
  original: String,
  draft: String,
  pending: String,
  filesCloudId: String
) extends BossSerializable {

  def isIdHere(contractId: String): Boolean = {
    List(original, draft, pending).contains(contractId)
  }

  override def toJS: ContractIdsJS =
    ContractIdsJS(
      uuid,
      name,
      original,
      draft,
      pending,
      filesCloudId)
}

case class ContractIdsJS(uuid: String,
                         name: String,
                         original: String,
                         draft: String,
                         pending: String,
                         filesCloudId: String)
  extends BossCase

//helper
object ContractBossConverter {
  def contractBinaryFromJS(serialized: BossCase): BossSerializable = {
    val jsClass = serialized.asInstanceOf[ContractBinaryJS]
    ContractBinary(jsClass.binary)
  }

  def ContractIdsFromJS(serialized: BossCase): BossSerializable = {
    val jsClass = serialized.asInstanceOf[ContractIdsJS]
    ContractIds(
      jsClass.uuid,
      jsClass.name,
      jsClass.original,
      jsClass.draft,
      jsClass.pending,
      jsClass.filesCloudId
    )
  }
}

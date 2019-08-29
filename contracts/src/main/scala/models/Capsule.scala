package models

import boss._
import boss.jsany._
import models.contracts.ContractType
import models.errors.{BalanceError, DefinitionChangeError, NoRoleError}
import org.scalajs.dom.raw.{Blob, BlobPropertyBag}
import tools.universa.UniversaTools._
import xchange._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportStatic, JSExportTopLevel}

case class UniversaContractJS(
  api_level: Int,
  definition: HashMap[String, Any],
  state: HashMap[String, Any],
  transactional: HashMap[String, Any]
) extends BossCase


@JSExportTopLevel("Universa.UniversaContract")
class UniversaContract (
  val apiLevel: Int,
  var definition: HashMap[String, Any],
  var state: HashMap[String, Any],
  var transactional: HashMap[String, Any]
) extends BossSerializable {

  def this(loaded: UniversaContractJS) = this(
    loaded.api_level,
    loaded.definition,
    loaded.state,
    loaded.transactional
  )

  def toJS: UniversaContractJS = {
    UniversaContractJS(apiLevel, definition, state, transactional)
  }

  def revision: Int = state("revision").asInstanceOf[Int]
  def setRevision(rev: Int): Unit = state("revision") = rev

  def setDefinitionData(path: String, value: Any): Unit = {
    val data = definition("data").asInstanceOf[HashMap[String, Any]]
    data(path) = value
  }

  @JSExport
  def getDefinitionData(path: String): Any = {
    val data = definition("data").asInstanceOf[HashMap[String, Any]]
    data(path)
  }

  def setStateData(path: String, value: Any): Unit = {
    val data = state("data").asInstanceOf[HashMap[String, Any]]
    data(path) = value
  }

  @JSExport
  def getStateData(path: String): Any = {
    val data = state("data").asInstanceOf[HashMap[String, Any]]
    data(path)
  }

  def addPermission(perm: Permission): Unit = {
    val id = perm.id
    val permissions = definition("permissions").asInstanceOf[HashMap[String, Permission]]
    permissions(id) = perm
  }

  def getAllPermissions[T <: Permission](): mutable.HashMap[String, Permission] = {
    definition("permissions").asInstanceOf[mutable.HashMap[String, Permission]]
  }

  def getPermission[T <: Permission](permissionName: String): Option[T] = {
    val permissions = definition("permissions").asInstanceOf[HashMap[String, Permission]]
    permissions.values.find(_.name == permissionName).map(_.asInstanceOf[T])
  }

  def getModifyDataPermission(): Option[ModifyDataPermission] = {
    getPermission[ModifyDataPermission]("modify_data")
  }

  def getSplitJoinPermission(): Option[SplitJoinPermission] = {
    getPermission[SplitJoinPermission]("split_join")
  }

  type mutableMap = mutable.HashMap[String, Any]

  def get(path: String): Any = {
    val steps = path.split("\\.")
    var prevResult: mutableMap = steps.head match {
      case "definition" => this.definition
      case "state" => this.state
      case "transactional" => this.transactional
    }

    steps.tail.dropRight(1).foreach {stepp =>
      prevResult = prevResult(stepp).asInstanceOf[mutableMap]
    }
    prevResult.getOrElse(steps.last, null)
  }

  def set(path: String, value: Any): Unit = {
    val steps = path.split("\\.")
    var prevResult: mutableMap = steps.head match {
      case "definition" => this.definition
      case "state" => this.state
      case "transactional" => this.transactional
    }
    steps.tail.dropRight(1).foreach {stepp =>
      val res = prevResult.get(stepp).map(_.asInstanceOf[mutableMap])
      prevResult = res.getOrElse{
        val map = new mutableMap
        prevResult put (stepp, map)
        map
      }
    }
    prevResult put (steps.last, value)
  }

  //.flatMap(Option(_)) is used to avoid Some(null)
  def getOrigin: Option[HashId] = state.get("origin").flatMap(Option(_)).asInstanceOf[Option[HashId]]
  def getParent: Option[HashId] = state.get("parent").flatMap(Option(_)).asInstanceOf[Option[HashId]]

  def setOrigin(capsuleBinary: Seq[Byte]): Unit =
    state("origin") = HashId(capsuleBinary)

  def setParent(capsuleBinary: Seq[Byte]): Unit =
    state("parent") = HashId(capsuleBinary)

  def setOrigin(id: HashId): Unit =
    state("origin") = id

  def setParent(id: HashId): Unit =
    state("parent") = id

  def getRoles: HashMap[String, Role] = {
    val roles = HashMap[String, Role]()

    UniversaContract.mainRoles.map(path => {
      val role = getMainRole(path)
      roles += (role.name -> role)
    })

    ListBuffer(definition, state).map(section => {
      if (!section.get("roles").isEmpty) {
        val rolesValue = section("roles")

        if (rolesValue.isInstanceOf[ListBuffer[_ <: Any]]) {
          rolesValue.asInstanceOf[ListBuffer[Role]].map(
            r => roles += (r.name -> r)
          )
        } else {
          roles ++= rolesValue.asInstanceOf[HashMap[String, Role]]
        }
      }
    })

    roles
  }

  private def getMainRole(path: String): Role =
    get(path).asInstanceOf[Role]

  def getRole(name: String): Option[Role] =
    //protection from Role = null
    getRoles.get(name).flatMap(Option(_))
}

object UniversaContract {
  val mainRoles = ListBuffer(
    "definition.issuer",
    "state.owner",
    "state.created_by"
  )

  def fromJS(serialized: BossCase): BossSerializable =
    new UniversaContract(serialized.asInstanceOf[UniversaContractJS])

  def apply(key: PublicKey, useLongAddress: Boolean): UniversaContract = {
    val issuer = new RoleSimple("issuer", key, useLongAddress)
    apply(issuer)
  }

  def apply(address: Seq[Byte]): UniversaContract = {
    val issuer = new RoleSimple("issuer", new KeyAddress(address))
    apply(issuer)
  }

  def apply(issuer: Role): UniversaContract = {
    val owner = new RoleLink("owner", "issuer")
    val creator = new RoleLink("creator", "issuer")
    val apiLevel = 4

    val createdAt = XChangeAPI.getNetworkJsDate()
    val expiresAt = new js.Date(createdAt.getTime)
    expiresAt.setFullYear(expiresAt.getFullYear() + 5)
    // expiresAt.setMonth(expiresAt.getMonth() + 3)

    val revoke = new RevokePermission("owner")
    val permissions = HashMap((revoke.id, revoke))

    val definition = HashMap(
      ("created_at", createdAt),
      ("issuer", issuer),
      ("permissions", permissions),
      ("data", HashMap[String, Any]())
    ).asInstanceOf[HashMap[String, Any]]
    val state = HashMap(
      ("created_at", createdAt),
      ("created_by", creator),
      ("revision", 1),
      ("owner", owner),
      ("expires_at", expiresAt),
      ("data", HashMap[String, Any]())
    ).asInstanceOf[HashMap[String, Any]]

    new UniversaContract(apiLevel, definition, state, null)
  }
}

@JSExportTopLevel("Universa.Capsule")
class CapsuleExported(val capsule: Capsule) extends js.Object {
  def toFile: Blob = {
    val content = js.Array(capsule.currentBinary.toJSArray)
    // val options = new BlobPropertyBag("application/octet-stream")
    val options = js.Dictionary(("type", "application/octet-stream")).asInstanceOf[BlobPropertyBag]
    new Blob(content.asInstanceOf[js.Array[js.Any]], options)
  }

  def lockData(): Unit = capsule.lockData()

  def lock(): Unit = capsule.lock()

  def currentBinary: js.Array[Byte] = capsule.currentBinary.toJSArray

  def setDefinition(path: String, value: js.Any): Unit = {
    if (capsule.revision != 1) throw new DefinitionChangeError("Cannot set definition for registered contract")
    capsule.setDefinition(path, Boss.read(value))
  }

  def setState(path: String, value: js.Any) = capsule.setState(path, Boss.read(value))

  def toBOSS(): js.Array[Byte] = capsule.toBOSS.toJSArray

  def toBase64(): String = encode64(capsule.toBOSS)

  def hashId(): js.Array[Byte] = capsule.hashId.composite3.toJSArray

  def currentHashId(): js.Array[Byte] = capsule.currentHashId.composite3.toJSArray

  def sign(key: PrivateKeyExported): Unit = capsule.sign(key.privateKey)

  def revision: Int = capsule.revision

  def createdAt: js.Date =
    capsule.getState("created_at").asInstanceOf[js.Date]

  def getRoles: js.Dictionary[Role] = capsule.getRoles.toJSDictionary

  def getRolesList(): js.Array[Role] = {
     capsule.getRoles.values.toSeq.toJSArray
  }

  def getRole(name: String): Role = capsule.getRole(name).orNull

  // def getRolesForPublicKey(key: PublicKeyExported): js.Array[Role] = {
  //   capsule.getRolesForPublicKey(key.publicKey).toJSArray
  // }

  // def getRolesForPair(pair: PairExported): js.Array[Role] = {
  //   capsule.getRolesForPair(pair.pair).toJSArray
  // }

  // def getRolesForUAddress(uaddress: js.Array[Byte]): js.Array[Role] = {
  //   capsule.getRolesForUAddress(uaddress.toSeq).toJSArray
  // }

  // def getRolesForKeyAddress(address: KeyAddress): js.Array[Role] = {
  //   capsule.getRolesForKeyAddress(address).toJSArray
  // }

  def getInfo(): js.Dictionary[_] = {
    val result = capsule.signaturesInfo
    js.Dictionary(
      "publicKeys" -> result._1.map(pk => new PublicKeyExported(pk)).toJSArray,
      "roles" -> result._2.toJSArray
    )
  }

  def isSignedBy(publicKey: PublicKeyExported): Boolean = {
    val addrs = capsule.signaturesInfo._1.map(_.shortAddress)
    addrs.contains(publicKey.publicKey.shortAddress)
  }

  def isRoleAvailable(roleName: String, publicKeys: js.Array[PublicKeyExported]): Boolean = {
    val role = getRole(roleName)
    Option(role).fold(false) { r =>
      r.availableForKeys(publicKeys.map(_.publicKey), getRolesList(), 0)
    }
  }

  def getAmount(): Double = capsule.getAmount.toDouble
  def getState(path: String): js.Any = Boss.write(capsule.getState(path))
  def getDefinition(path: String): js.Any = Boss.write(capsule.getDefinition(path))

  def getPermissions(): js.Dictionary[js.Dictionary[Any]] = {
    capsule.contract.getAllPermissions().map{case (k, v) => (v.name, v.getParams())}.toJSDictionary
  }

  def setTTL(amount: Int, unit: String): Unit = {
    capsule.setTTL(amount, unit)
  }
}

class Capsule(val originalBinary: Seq[Byte]) {
  val unicapsule = Boss.load(originalBinary).asInstanceOf[HashMap[String, Any]]
  var version = unicapsule("version").asInstanceOf[Int]
  var signatures = unicapsule("signatures").asInstanceOf[ListBuffer[Seq[Byte]]]
  private var packedData = unicapsule("data").asInstanceOf[Seq[Byte]]
  private val data = Boss.load(packedData).asInstanceOf[HashMap[String, Any]]
  val contract = data("contract").asInstanceOf[UniversaContract]
  var revokingIds = data("revoking").asInstanceOf[ListBuffer[HashId]]
  var newIds = data("new").asInstanceOf[ListBuffer[HashId]]
  var currentBinary = originalBinary

  def getData: Seq[Byte] = Boss.dump(HashMap(
    ("new", newIds),
    ("revoking", revokingIds),
    ("contract", contract)
  ))

  def toBOSS: Seq[Byte] = Boss.dump(HashMap(
    ("type", "unicapsule"),
    ("version", version),
    ("data", packedData),
    ("signatures", signatures)
  ))

  def getDefinition(path: String): Any = contract.get(s"definition.$path")
  def getState(path: String): Any = contract.get(s"state.$path")
  def getField(path: String): Any = contract.get(path)
  def setDefinition(path: String, value: Any): Unit = contract.set(s"definition.$path", value)
  def setState(path: String, value: Any): Unit = contract.set(s"state.$path", value)
  def setTransactional(path: String, value: Any): Unit = {
    if (contract.transactional == null) contract.transactional = HashMap[String, Any]()
    contract.set(s"transactional.$path", value)
  }
  def getTransactional(path: String): Any = contract.get(s"transactional.$path")
  def addReference(ref: Reference): Unit = {
    var references = getTransactional("references").asInstanceOf[ListBuffer[Reference]]
    if (references == null) references = ListBuffer[Reference]()

    references += ref
    setTransactional("references", references)
  }

  def copyAsRoot: Capsule = {
    val cap = new Capsule(currentBinary)
    cap.contract.setRevision(1)
    cap.contract.set("state.origin", null)
    cap.contract.set("state.parent", null)


    val creator = new RoleLink("creator", "issuer")
    val createdAt = XChangeAPI.getNetworkJsDate()
    val expiresAt = new js.Date(createdAt.getTime)
    // expiresAt.setFullYear(expiresAt.getFullYear() + 5)
    expiresAt.setMonth(expiresAt.getMonth() + 3)

    cap.contract.set("state.created_by", creator)
    cap.contract.set("state.created_at", createdAt)
    cap.contract.set("state.expires_at", expiresAt)
    cap
  }

  def lockData(): Unit = {
    packedData = getData
    signatures = ListBuffer[Seq[Byte]]()
  }

  def lock(): Unit = currentBinary = toBOSS

  def lockDataAndSign(signKeys: PrivateKey*): Unit = this.synchronized {
    lockData()
    signKeys.foreach(sign)
    lock()
  }

  def resetSignatures: Unit = {
    signatures = ListBuffer[Seq[Byte]]()
    lock
  }

  lazy val hashId: HashId = HashId(originalBinary)
  lazy val uuid = hashId.base64
  def currentHashId: HashId = HashId(currentBinary)

  def sign(key: PrivateKey): Unit = signatures += key.signExtended(packedData)
  def sign(keys: Seq[PrivateKey]): Unit =
    keys.foreach(key => signatures += key.signExtended(packedData))

  def this(encoded: js.Array[Byte]) = this(encoded.toSeq)

  def revision: Int = contract.revision

  def isParentFor(child: Capsule): Boolean = {
    child.getParent match {
      case None => false
      case Some(parent) => {
        currentHashId.equals(parent)
      }
    }
  }

  def isOriginFor(child: Capsule): Boolean = {
    child.getOrigin match {
      case None => false
      case Some(parent) => {
        currentHashId.equals(parent)
      }
    }
  }

  def setRevision(rev: Int): Unit = contract.setRevision(rev)
  def getOrigin: Option[HashId] = contract.getOrigin
  def getParent: Option[HashId] = contract.getParent

  def setCreatorLink(roleName: String): Unit =
    contract.set("state.created_by", new RoleLink("creator", roleName))

  def setOwner(address: KeyAddress): Unit = {
    setState("owner", new RoleSimple("owner", address))
  }

  def setOwner(key: PublicKey): Unit = {
    setState("owner", new RoleSimple("owner", key))
  }

  def setOrigin(capsuleBinary: Seq[Byte]): Unit =
    contract.setOrigin(capsuleBinary)

  def setParent(capsuleBinary: Seq[Byte]): Unit =
    contract.setParent(capsuleBinary)

  def setOrigin(id: HashId): Unit = contract.setOrigin(id)
  def setParent(id: HashId): Unit = contract.setParent(id)

  def setRevokingContracts(contracts: ListBuffer[Contract]): Unit = {
    revokingIds = contracts.map(_.original.currentHashId)
  }

  def resetRevokingContracts: Unit = revokingIds = ListBuffer[HashId]()
  def resetNewContracts: Unit = newIds = ListBuffer[HashId]()

  def setNewContracts(contracts: ListBuffer[Contract]): Unit = {
    newIds = contracts.map(_.original.currentHashId)
  }

  def addNewCapsules(capsules: ListBuffer[Capsule]): Unit = {
    newIds ++= capsules.map(_.currentHashId)
  }

  def getRoles: HashMap[String, Role] = contract.getRoles

  def getRole(name: String): Option[Role] = contract.getRole(name)

  def getFinalRole(name: String): Role = {
    var finalRole = getRole(name).get

    while (finalRole.isInstanceOf[RoleLink])
      finalRole = getRole(finalRole.asInstanceOf[RoleLink].targetName).get

    finalRole
  }

  def getKeyForRole(
    roleName: String,
    privateKeys: mutable.Seq[PrivateKey]
  ): Option[PrivateKey] = {
    val rolesList = getRoles.values.toSeq
    val roles = mutable.Seq[Role](rolesList: _*)
    val roleOpt = getRole(roleName)

    roleOpt match {
      case None => throw new NoRoleError(s"There's no role $roleName in contract")
      case Some(role) =>
        privateKeys.find(k => role.availableForKeys(ListBuffer(k.publicKey), roles, 0))
    }
  }

  // def getRolesForPublicKey(key: PublicKey): Seq[Role] = {
  //   val keys = mutable.Seq[PublicKey](key)
  //   val allRoles = getRoles.values.toSeq
  //   allRoles.filter(v => v.availableForKeys(keys, mutable.Seq(allRoles: _*)))
  // }

  // def getRolesForPair(pair: Pair): Seq[Role] = {
  //   getRolesForPublicKey(pair.publicKey).toJSArray
  // }

  // def getRolesForUAddress(uaddress: Seq[Byte]): Seq[Role] = {
  //   getRolesForKeyAddress(new KeyAddress(uaddress))
  // }

  // def getRolesForKeyAddress(address: KeyAddress): Seq[Role] = {
  //   val addresses = mutable.Seq[KeyAddress](address)
  //   getRoles.values.toSeq.filter(v => v.availableFor(mutable.Seq.empty[PublicKey], addresses))
  // }

  def getPublicKeys: Seq[PublicKey] = {
    signatures
      .view
      .flatMap(loaded => Boss.load(loaded).asInstanceOf[HashMap[String, Seq[Byte]]].get("exts"))
      .flatMap(exts => Boss.load(exts).asInstanceOf[HashMap[String, Seq[Byte]]].get("pub_key"))
      .map(key => new PublicKey(PublicKeyJS(key)))
      .force
  }

  // def getPublicKeysWithRoles: (Seq[PublicKey], Seq[Role]) = {
  //   val keys = getPublicKeys
  //   val roles = keys.flatMap(key => getRolesForPublicKey(key))
  //   (keys, roles)
  // }

  def copy(): Capsule = new Capsule(currentBinary)

  def getModifyData: Option[ModifyDataPermission] = contract.getModifyDataPermission()
  def getSplitJoin: Option[SplitJoinPermission] =  contract.getSplitJoinPermission()

  def resetTransactional: Unit = {
    contract.transactional = null
  }

  def clearTransactional: Unit = {
    contract.transactional = HashMap[String, Any]()
  }

  def getAmount: BigDecimal = {
    // FIXME: in old contracts there is Number in it
    var amountString = getState("data.amount").toString

    if (amountString != null) BigDecimal(amountString.asInstanceOf[String])
    else throw new BalanceError("No amount field found in contract")
  }

  def setAmount(amount: BigDecimal): Unit = {
    val minUnitPermission = contract.getSplitJoinPermission().flatMap{p =>
      if (p.fieldName == "amount")
        p.minUnit.toStringOpt.map(v => BigDecimal(v))
      else None
    }
    val newAmount: BigDecimal = CapsuleExported.getProperAmount(amount, minUnitPermission)
    setState("data.amount", newAmount.bigDecimal.toString)
  }

  def setRoleByKey(roleName: String, publicKey: PublicKey, useLongAddress: Boolean): Unit = {
    val role = new RoleSimple(roleName, publicKey, useLongAddress)
    val path = if (roleName == "creator") "created_by" else roleName
    if (roleName == "owner" || roleName == "creator") setState(path, role)
    else setDefinition(path, role)
  }

  def signaturesInfo: (Seq[PublicKey], Seq[Role]) = {
    // FIXME: useless conversion
    val keys = getPublicKeys.asInstanceOf[mutable.Seq[PublicKey]]
    val rolesList = getRoles.values.toSeq
    val rolesSigned = rolesList.filter(v =>
      v.availableFor(keys, mutable.Seq[KeyAddress](), rolesList)
    )
    (keys, rolesSigned)
  }

  def setTTL(amount: Int, unit: String): Unit = {
    val createdAt = getDefinition("created_at").asInstanceOf[js.Date]
    val expiresAt = new js.Date(createdAt.getTime)
    if (unit == "month")
      expiresAt.setMonth(expiresAt.getMonth() + amount)

    if (unit == "year")
      expiresAt.setFullYear(expiresAt.getFullYear() + amount)

    if (unit == "day")
      expiresAt.setDate(expiresAt.getDate() + amount)

    setState("expires_at", expiresAt)
  }

}

object Capsule {
  def apply(publicKey: PublicKey, useLongAddress: Boolean = false): Capsule = {
    val data = Boss.dump(HashMap(
      ("new", ListBuffer[Seq[Byte]]()),
      ("revoking", ListBuffer[Seq[Byte]]()),
      ("contract", UniversaContract(publicKey, useLongAddress))
    ))

    val packedCapsule = Boss.dump(HashMap(
      ("type", "unicapsule"),
      ("version", 4),
      ("data", data),
      ("signatures", ListBuffer[Seq[Byte]]())
    ))

    new Capsule(packedCapsule)
  }

  def apply(address: Seq[Byte]): Capsule = {
    val data = Boss.dump(HashMap(
      ("new", ListBuffer[Seq[Byte]]()),
      ("revoking", ListBuffer[Seq[Byte]]()),
      ("contract", UniversaContract(address))
    ))

    val packedCapsule = Boss.dump(HashMap(
      ("type", "unicapsule"),
      ("version", 4),
      ("data", data),
      ("signatures", ListBuffer[Seq[Byte]]())
    ))

    new Capsule(packedCapsule)
  }

   def apply(role: Role): Capsule = {
    val data = Boss.dump(HashMap(
      ("new", ListBuffer[Seq[Byte]]()),
      ("revoking", ListBuffer[Seq[Byte]]()),
      ("contract", UniversaContract(role.copyWithName("issuer")))
    ))

    val packedCapsule = Boss.dump(HashMap(
      ("type", "unicapsule"),
      ("version", 4),
      ("data", data),
      ("signatures", ListBuffer[Seq[Byte]]())
    ))

    new Capsule(packedCapsule)
  }
}

object CapsuleExported {

  def getProperAmount(amount: BigDecimal, minUnitOpt: Option[BigDecimal] = None): BigDecimal = {
    minUnitOpt match {
      case None => amount
      case Some(minUnit) =>
        val minUnitScale = minUnit.scale
        val maxScale18 = Math.min(18, minUnitScale)
        val maxPrecision = Math.pow(10, maxScale18).toLong

        val d = amount * maxPrecision % (minUnit * maxPrecision)
        val newAmount = ((amount * maxPrecision - d) / maxPrecision).setScale(maxScale18)
        newAmount
    }
  }

  @JSExportStatic("fromBOSS")
  def fromBOSS(encoded: js.Array[Byte]): CapsuleExported = {
    val capsule = new Capsule(encoded)
    new CapsuleExported(capsule)
  }

  @JSExportStatic("fromBase64")
  def fromBase64(encoded: String): CapsuleExported = {
    val capsule = new Capsule(decode64(encoded))
    new CapsuleExported(capsule)
  }

  @JSExportStatic("createByKey")
  def createByKeyJS(key: PublicKeyExported): CapsuleExported = {
    val capsule = createByKey(key)
    new CapsuleExported(capsule)
  }

  @JSExportStatic("createByKey")
  def createByKeyJS(key: PublicKeyExported, useLongAddress: Boolean): CapsuleExported = {
    val capsule = createByKey(key, useLongAddress)
    new CapsuleExported(capsule)
  }

  def createByKey(key: PublicKeyExported, useLongAddress: Boolean = false): Capsule = {
    val data = Boss.dump(HashMap(
      ("new", ListBuffer[Seq[Byte]]()),
      ("revoking", ListBuffer[Seq[Byte]]()),
      ("contract", UniversaContract(key.publicKey, useLongAddress))
    ))

    val packedCapsule = Boss.dump(HashMap(
      ("type", "unicapsule"),
      ("version", 4),
      ("data", data),
      ("signatures", ListBuffer[Seq[Byte]]())
    ))

    new Capsule(packedCapsule)
  }

  def createByAddress(address: Seq[Byte]): Capsule = {
    val data = Boss.dump(HashMap(
      ("new", ListBuffer[Seq[Byte]]()),
      ("revoking", ListBuffer[Seq[Byte]]()),
      ("contract", UniversaContract(address))
    ))

    val packedCapsule = Boss.dump(HashMap(
      ("type", "unicapsule"),
      ("version", 4),
      ("data", data),
      ("signatures", ListBuffer[Seq[Byte]]())
    ))

    new Capsule(packedCapsule)
  }

  @JSExportStatic("createToken")
  def createTokenJS(
                   key: PublicKeyExported,
                   amount: String,
                   minUnit: Double,
                   unitFull: String,
                   unitShort: String,
                   description: String
                 ): CapsuleExported = {
    val cap = createToken(key, amount, minUnit, unitFull, unitShort, description, false)
    new CapsuleExported(cap)
  }

  def createToken(
    key: PublicKeyExported,
    amount: String,
    minUnit: Double,
    unitFull: String,
    unitShort: String,
    description: String,
    useLongAddress: Boolean = false
  ): Capsule = {
    val cap = createByKey(key, useLongAddress)
    cap.contract.setDefinitionData("template_name", ContractType.UNIT_CONTRACT.toString)
    cap.contract.setDefinitionData("unit_name", unitFull)
    cap.contract.setDefinitionData("unit_short_name", unitShort)
    cap.contract.setDefinitionData("description", description)

    val splitJoin = SplitJoinPermission(
      "owner",
      minUnit.toString,
      minUnit.toString,
      "amount",
      ListBuffer("state.origin")
    )
    cap.contract.addPermission(splitJoin)
    cap.contract.addPermission(ChangeOwnerPermission("owner"))
    //this must come after permissions as permission-minUnit make influence on amount!
    cap.setAmount(BigDecimal(amount))
    cap
  }
}

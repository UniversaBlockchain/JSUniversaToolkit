package models

import boss._
import boss.jsany._

import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

import tools.universa.UniversaTools._

abstract class Role extends BossSerializable {
  @JSExport
  val name: String

  def availableForKeys(
    keysToCheck: mutable.Seq[PublicKey],
    roles: Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Boolean = {
    availableFor(keysToCheck, mutable.Seq[KeyAddress](), roles, nestedLevel)
  }

  def availableForAddresses(
    addressesToCheck: mutable.Seq[KeyAddress],
    roles: mutable.Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Boolean = {
    availableFor(mutable.Seq[PublicKey](), addressesToCheck, roles, nestedLevel)
  }

  def availableFor(
    keysToCheck: mutable.Seq[PublicKey],
    addressesToCheck: mutable.Seq[KeyAddress],
    roles: Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Boolean

  def getAddress58(
    long: Boolean = false,
    roles: mutable.Seq[Role] = ListBuffer.empty[Role]
  ): String = encode58(getAddress(long, roles))

  def getAddress(
    long: Boolean = false,
    roles: mutable.Seq[Role] = ListBuffer.empty[Role]
  ): Seq[Byte]

  def copyWithName(name: String): Role

  def resolve(
    roles: mutable.Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Role
}

case class RoleSimpleJS(
	name: String,
  keys: Any,
  addresses: Any
) extends BossCase

// object RoleSimpleJS {
//   def apply(name: String, keys: ListBuffer[KeyRecord], addresses: ListBuffer[Seq[Byte]]): RoleSimpleJS = {

//   }
// }

/**
 * Simple role with array of public keys or addresses
 *
 * @constructor create role by name
 * @param name - role name
 */
@JSExportTopLevel("Universa.RoleSimple")
class RoleSimple(val name: String) extends Role {
	var keys = ListBuffer[PublicKey]()
	var addresses = ListBuffer[KeyAddress]()

  def this(name: String, address: KeyAddress) = {
    this(name)
    addresses = ListBuffer(address)
  }

  def this(name: String, key: PublicKey, long: Boolean = false) = {
    this(name)
    addresses = ListBuffer(new KeyAddress(key, long))
  }

  def this(loaded: RoleSimpleJS) = {
    this(loaded.name)

    val records = loaded.keys
    val addrs = loaded.addresses

    keys = records match {
      case null => ListBuffer[PublicKey]()
      case _ => records.asInstanceOf[ListBuffer[KeyRecord]] map { _.key }
    }

    addresses = addrs match {
      case null => ListBuffer[KeyAddress]()
      case _ => addrs.asInstanceOf[ListBuffer[KeyAddress]]
    }
  }

  def this(bossEncoded: Seq[Byte]) = {
    this(Boss.load(bossEncoded).asInstanceOf[RoleSimpleJS])
  }

  def copyWithName(newName: String): RoleSimple = {
    val newRole = new RoleSimple(newName)
    newRole.keys = keys
    newRole.addresses = addresses
    newRole
  }

  def resolve(
    roles: mutable.Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Role = this

  /** Pack to JS-compatible structure */
  def toJS: RoleSimpleJS = {
    // RoleSimpleJS(name, Some(keys map { k => new KeyRecord(k) }), addresses)
    RoleSimpleJS(name, keys map { k => new KeyRecord(k) }, addresses)
  }

  /**
   * Check if role is available for given keys
   *
   * @param exportedKeysToCheck - array of public keys to check
   * @param roles - array of roles to check links
   */
  @JSExport("availableForKeys")
  def availableForKeysJS(
                          exportedKeysToCheck: js.Array[PublicKeyExported],
                          roles: js.Array[Role] = js.Array[Role](),
                          nestedLevel: Int = 0
  ): Boolean = {
    val keysToCheck = exportedKeysToCheck.map(_.publicKey)
    availableForKeys(keysToCheck, roles, nestedLevel)
  }

  /**
   * Check if role is available for given addresses
   *
   * @param addressesToCheck - array of addresses
   * @param roles - array of roles to check links
   */
  @JSExport("availableForAddresses")
  def availableForAddressesJS(
    addressesToCheck: js.Array[KeyAddress],
    roles: js.Array[Role] = js.Array[Role](),
    nestedLevel: Int = 0
  ): Boolean = {
    availableForAddresses(addressesToCheck, roles, nestedLevel)
  }

  /**
   * Check if role is available for given keys or/and addresses
   *
   * @param exportedKeysToCheck - array of public keys to check
   * @param addressesToCheck - array of addresses
   * @param roles - array of roles to check links
   */
  @JSExport("availableFor")
  def availableForJS(
    exportedKeysToCheck: js.Array[PublicKeyExported],
    addressesToCheck: mutable.Seq[KeyAddress],
    roles: js.Array[Role] = js.Array[Role](),
    nestedLevel: Int = 0
  ): Boolean = {
    val keysToCheck = exportedKeysToCheck.map(_.publicKey)
    availableFor(keysToCheck, addressesToCheck, roles, nestedLevel)
  }

  override def availableFor(
    keysToCheck: mutable.Seq[PublicKey],
    addressesToCheck: mutable.Seq[KeyAddress],
    roles: Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Boolean = {
    // println(s"running availableFor, ${this.name} level $nestedLevel")
    if (nestedLevel >= 100) throw new Exception(s"circular role ${this.name}")

    val prints = keysToCheck map { _.fingerprint }
    val rolePrints = keys map { _.fingerprint }

    if (rolePrints.nonEmpty && rolePrints.toSet.subsetOf(prints.toSet)) return true

    val kAddressesLong = keysToCheck map { k => new KeyAddress(k, true).uaddress }
    val kAddressesShort = keysToCheck map { k => new KeyAddress(k, false).uaddress }
    val kAddresses = kAddressesShort ++ kAddressesLong
    val aAddresses = addressesToCheck map { k => k.uaddress }

    val localAddresses = addresses map { k => k.uaddress }

    if (localAddresses.nonEmpty && (
      localAddresses.toSet.subsetOf(kAddresses.toSet) ||
      localAddresses.toSet.subsetOf(aAddresses.toSet)
    )) return true

    false
  }

  def getRawAddresses(long: Boolean = false): ListBuffer[Seq[Byte]] = {
    val list = addresses map { k => k.uaddress }
    val plist = keys map { k => new KeyAddress(k, long).uaddress }
    list ++= plist
    list
  }

  def getAddress(
    long: Boolean = false,
    roles: mutable.Seq[Role] = ListBuffer.empty[Role]
  ): Seq[Byte] = {
    val addrs = getRawAddresses(long)
    addrs.head
  }

  /**
   * Check if role is available for key
   *
   * @param key - public key to check
   */
  def availableForKey(key: PublicKey): Boolean = {
    availableForKeys(mutable.Seq(key))
  }
}

object RoleSimple {
  def fromJS(serialized: BossCase): BossSerializable = {
    new RoleSimple(serialized.asInstanceOf[RoleSimpleJS])
  }
}

case class RoleLinkJS(
  name: String,
  target_name: String
) extends BossCase

/**
 * Role link that references other role
 *
 * @constructor create role by name
 * @param name - role name
 * @param targetName - target role name
 */
@JSExportTopLevel("Universa.RoleLink")
class RoleLink(val name: String, val targetName: String) extends Role {

  def this(jsVal: RoleLinkJS) = this(jsVal.name, jsVal.target_name)

  def this(bossEncoded: Seq[Byte]) =
    this(Boss.load(bossEncoded).asInstanceOf[RoleLinkJS])

  /** Pack to JS-compatible structure */
  def toJS: RoleLinkJS = RoleLinkJS(name, targetName)

  /**
   * Check if role is available for given keys or addresses
   *
   * @param keysToCheck - array of public keys to check
   * @param addressesToCheck - array of key addresses to check
   * @param roles - array of roles to check links
   * @param nestedLevel - level of deepness (to check in case of circular role error)
   */
  override def availableFor(
                        keysToCheck: mutable.Seq[PublicKey],
                        addressesToCheck: mutable.Seq[KeyAddress],
                        roles: Seq[Role] = ListBuffer.empty[Role],
                        nestedLevel: Int = 0
  ): Boolean = {
    // println(s"running availableFor, ${this.name} level $nestedLevel")
    if (nestedLevel >= 100) throw new Exception(s"circular role ${this.name}")

    roles
      .find(_.name == targetName)
      .fold(false)(_.availableFor(keysToCheck, addressesToCheck, roles, nestedLevel + 1))
  }

  def resolve(
    roles: mutable.Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Role = {
    if (nestedLevel >= 100) throw new Exception(s"circular role ${this.name}")

    val target = roles.find(_.name == targetName)
    if (target.isEmpty) throw new Exception("Role target not found")

    target.get.resolve(roles, nestedLevel + 1)
  }


  /**
   * Check if role is available for given keys
   *
   * @param exportedKeysToCheck - array of public keys to check
   * @param roles - array of roles to check links
   */
  @JSExport("availableForKeys")
  def availableForKeysJS(
                          exportedKeysToCheck: js.Array[PublicKeyExported],
                          roles: js.Array[Role] = js.Array[Role](),
                          nestedLevel: Int = 0
  ): Boolean = {
    val keysToCheck = exportedKeysToCheck.map(_.publicKey)
    roles.find(_.name == targetName).fold(false)(_.asInstanceOf[RoleSimple].availableForKeys(keysToCheck, roles, nestedLevel + 1))
  }

  def getAddress(
    long: Boolean = false,
    roles: mutable.Seq[Role] = ListBuffer.empty[Role]
  ): Seq[Byte] = {
    val target = roles.find(_.name == targetName).asInstanceOf[Option[Role]]
    target.get.getAddress(long, roles)
  }

  def copyWithName(newName: String): RoleLink = new RoleLink(newName, targetName)
}

object RoleLink {
  def fromJS(serialized: BossCase): BossSerializable = {
    new RoleLink(serialized.asInstanceOf[RoleLinkJS])
  }
}

case class RoleListJS(
  name: String,
  mode: String,
  roles: ListBuffer[Role],
  quorumSize: Int
) extends BossCase

/**
 * Role that makes quorum from other roles
 *
 * @constructor create role by name, list of roles and quorum params
 * @param name - role name
 * @param quorumSize - number of chained roles that needed to make role available
 * @param mode - (ANY, ALL, QUORUM) - mode of list acceptance
 * @param roles - list of roles
 */
@JSExportTopLevel("Universa.RoleList")
class RoleList(
  val name: String,
  mode: String,
  roles: ListBuffer[Role],
  quorumSize: Int = 1
) extends Role {
  /** Pack to JS-compatible structure */
  def toJS: RoleListJS =
    RoleListJS(name, mode, roles, quorumSize)

  def this(jsVal: RoleListJS) =
    this(jsVal.name, jsVal.mode, jsVal.roles, jsVal.quorumSize)

  /**
   * Check if role is available for given keys or addresses
   *
   * @param keysToCheck - array of public keys to check
   * @param addressesToCheck - array of key addresses to check
   * @param roles - array of roles to check links
   * @param nestedLevel - level of deepness (to check in case of circular role error)
   */
  override def availableFor(
                        keysToCheck: mutable.Seq[PublicKey],
                        addressesToCheck: mutable.Seq[KeyAddress],
                        allRoles: Seq[Role] = ListBuffer.empty[Role],
                        nestedLevel: Int = 0
  ): Boolean = {
    // println(s"running availableFor, ${this.name} level $nestedLevel")
    if (nestedLevel >= 100) throw new Exception(s"circular role ${this.name}")

    mode match {
      case "ANY" => roles.exists(_.availableFor(keysToCheck, addressesToCheck, allRoles, nestedLevel + 1) == true)
      case "ALL" => roles.count(_.availableFor(keysToCheck, addressesToCheck, allRoles, nestedLevel + 1) == false) == 0
      case "QUORUM" => roles.count(_.availableFor(keysToCheck, addressesToCheck, allRoles, nestedLevel + 1) == true) >= quorumSize
    }
  }

  def getAddress(
    long: Boolean = false,
    roles: mutable.Seq[Role] = ListBuffer.empty[Role]
  ): Seq[Byte] = Seq[Byte]()

  def copyWithName(newName: String): RoleList =
    new RoleList(newName, mode, roles, quorumSize)

  def resolve(
    allRoles: mutable.Seq[Role] = ListBuffer.empty[Role],
    nestedLevel: Int = 0
  ): Role = {
    new RoleList(name, mode, roles.map(_.resolve(allRoles)))
  }
}

object RoleList {
  def fromJS(serialized: BossCase): BossSerializable = {
    new RoleList(serialized.asInstanceOf[RoleListJS])
  }
}

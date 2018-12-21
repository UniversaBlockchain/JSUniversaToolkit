package models

import boss._
import boss.jsany._
import scala.scalajs.js
import scala.collection.mutable.ListBuffer
import tools.universa.UniversaTools._
import scala.scalajs.js.JSConverters._

abstract class Permission extends BossSerializable {
  val id: String
  var role: Role
  val name: String

  def getParams(): js.Dictionary[Any]
}

case class RevokePermissionJS(
                               name: String,
                               role: Role
                             ) extends BossCase

object RevokePermission {
  def fromJS(serialized: BossCase): BossSerializable = {
    new RevokePermission(serialized.asInstanceOf[RevokePermissionJS])
  }
}

/**
  * Revoke permission
  *
  * @constructor create a new permission with linked role name
  */
class RevokePermission(
                        val id: String,
                        var role: Role,
                        val name: String = "revoke"
                      ) extends Permission {
  def this(roleName: String) =
    this(shortId, new RoleLink(s"@revoke", roleName))

  def this(jsEncoded: RevokePermissionJS) =
    this("", jsEncoded.role, jsEncoded.name)

  def this(encoded: Seq[Byte]) =
    this(Boss.load(encoded).asInstanceOf[RevokePermissionJS])

  /** Pack to JS-compatible structure */
  def toJS: RevokePermissionJS = RevokePermissionJS(name, role)

  override def getParams(): js.Dictionary[Any] = js.Dictionary[Any](
  )
}

case class ChangeNumberPermissionJS(
                                     name: String,
                                     role: Role,
                                     field_name: String,
                                     min_value: Option[Any],
                                     min_step: Option[Any],
                                     max_step: Option[Any],
                                     max_value: Option[Any]
                                   ) extends BossCase

object ChangeNumberPermission {
  def fromJS(serialized: BossCase): BossSerializable = {
    new ChangeNumberPermission(serialized.asInstanceOf[ChangeNumberPermissionJS])
  }
}

class ChangeNumberPermission(
                              val id: String,
                              var role: Role,
                              val name: String = "decrement_permission"
                            ) extends Permission {
  var fieldName: String = ""
  var minValue: StringOrDouble = new StringOrDouble(None)
  var minStep: StringOrDouble = new StringOrDouble(None)
  var maxValue: StringOrDouble = new StringOrDouble(None)
  var maxStep: StringOrDouble = new StringOrDouble(None)

  def this(jsEncoded: ChangeNumberPermissionJS) = {
    this("", jsEncoded.role, jsEncoded.name)

    fieldName = jsEncoded.field_name

    minValue = new StringOrDouble(jsEncoded.min_value)
    maxValue = new StringOrDouble(jsEncoded.max_value)
    minStep = new StringOrDouble(jsEncoded.min_step)
    maxStep = new StringOrDouble(jsEncoded.max_step)
  }

  def toJS: ChangeNumberPermissionJS = {
    ChangeNumberPermissionJS(name, role, fieldName, minValue.value, minStep.value, maxStep.value, maxValue.value)
  }

  override def getParams(): js.Dictionary[Any] = js.Dictionary[Any](
    "minValue" -> minValue.toString,
    "minStep" -> minStep.toString,
    "maxStep" -> maxStep.toString,
    "maxValue" -> maxValue.toString,
  )
}

case class SplitJoinPermissionJS(
                                  name: String,
                                  role: Role,
                                  field_name: String,
                                  min_value: Any,
                                  min_unit: Any,
                                  join_match_fields: ListBuffer[String]
                                ) extends BossCase

object SplitJoinPermission {
  def fromJS(serialized: BossCase): BossSerializable = {
    new SplitJoinPermission(serialized.asInstanceOf[SplitJoinPermissionJS])
  }

  def apply(
             roleName: String,
             minValue: String,
             minUnit: String,
             fieldName: String,
             joinMatchFields: ListBuffer[String]
           ): SplitJoinPermission = {
    val permission = new SplitJoinPermission(
      shortId,
      new RoleLink(s"@split_join", roleName)
    )
    permission.fieldName = fieldName
    permission.minValue = new StringOrDouble(Option(minValue))
    permission.minUnit = new StringOrDouble(Option(minUnit))
    permission.joinMatchFields = joinMatchFields
    permission
  }
}

class SplitJoinPermission(
                           val id: String,
                           var role: Role,
                           val name: String = "split_join"
                         ) extends Permission {
  var fieldName: String = ""
  var minValue: StringOrDouble = new StringOrDouble(Option("1"))
  var minUnit: StringOrDouble = new StringOrDouble(Option("1"))
  var joinMatchFields = ListBuffer[String]()

  def this(jsEncoded: SplitJoinPermissionJS) = {
    this("", jsEncoded.role, jsEncoded.name)
    fieldName = jsEncoded.field_name
    minValue = new StringOrDouble(Option(jsEncoded.min_value))
    minUnit = new StringOrDouble(Option(jsEncoded.min_unit))
    joinMatchFields = jsEncoded.join_match_fields
  }

  def toJS: SplitJoinPermissionJS = {
    SplitJoinPermissionJS(name, role, fieldName, minValue.value.orNull, minUnit.value.orNull, joinMatchFields)
  }

  override def getParams(): js.Dictionary[Any] = js.Dictionary[Any](
    "fieldName" -> fieldName,
    "minValue" -> minValue.toString,
    "minUnit" -> minUnit.toString,
    "joinMatchFields" -> joinMatchFields.toJSArray,
  )
}

case class ChangeOwnerPermissionJS(
                                    name: String,
                                    role: Role
                                  ) extends BossCase

object ChangeOwnerPermission {
  def fromJS(serialized: BossCase): BossSerializable = {
    new ChangeOwnerPermission(serialized.asInstanceOf[ChangeOwnerPermissionJS])
  }

  def apply(
             roleName: String
           ): ChangeOwnerPermission = {
    new ChangeOwnerPermission(
      shortId,
      new RoleLink(s"@change_owner", roleName)
    )
  }
}

class ChangeOwnerPermission(
                             val id: String,
                             var role: Role,
                             val name: String = "change_owner"
                           ) extends Permission {
  def this(roleName: String) =
    this(shortId, new RoleLink(s"@change_owner", roleName))

  def this(jsEncoded: ChangeOwnerPermissionJS) =
    this("", jsEncoded.role, jsEncoded.name)

  def toJS: ChangeOwnerPermissionJS = ChangeOwnerPermissionJS(name, role)

  override def getParams(): js.Dictionary[Any] = js.Dictionary[Any](
  )
}

case class ModifyDataPermissionJS(
                                   role: Role,
                                   name: String
                                 ) extends BossCase

object ModifyDataPermission {
  def fromJS(serialized: BossCase): BossSerializable = {
    ModifyDataPermission(serialized.asInstanceOf[ModifyDataPermissionJS])
  }

  def apply(jsEncoded: ModifyDataPermissionJS): ModifyDataPermission =
    new ModifyDataPermission("", jsEncoded.role, jsEncoded.name)

  def apply(roleName: String): ModifyDataPermission =
    new ModifyDataPermission(shortId, new RoleLink(s"@modify_data", roleName))
}

class ModifyDataPermission(
                            val id: String,
                            var role: Role,
                            val name: String = "modify_data"
                          ) extends Permission {

  def toJS: ModifyDataPermissionJS = ModifyDataPermissionJS(role, name)

  override def getParams(): js.Dictionary[Any] = js.Dictionary[Any](
  )
}

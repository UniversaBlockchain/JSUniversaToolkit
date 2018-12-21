package models.errors

import cloud.EncryptionError

import scala.collection.mutable.HashMap
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Universa.BalanceError")
class BalanceError(
                    override val message: String = "Not enough U to pay",
                    override val details: HashMap[String, Any] = HashMap[String, Any]()
                  ) extends EncryptionError(message, details)

@JSExportTopLevel("Universa.NoKeyFound")
class NoKeyError(
                  override val message: String,
                  override val details: HashMap[String, Any] = HashMap[String, Any]()
                ) extends EncryptionError(message, details)

@JSExportTopLevel("Universa.NoContractFound")
class NoContractError(
                       override val message: String,
                       override val details: HashMap[String, Any] = HashMap[String, Any]()
                     ) extends EncryptionError(message, details)

@JSExportTopLevel("Universa.NoRoleFound")
class NoRoleError(
                   override val message: String,
                   override val details: HashMap[String, Any] = HashMap[String, Any]()
                 ) extends EncryptionError(message, details)

@JSExportTopLevel("Universa.NoPermissionFound")
class NoPermissionError(
                         override val message: String,
                         override val details: HashMap[String, Any] = HashMap[String, Any]()
                       ) extends EncryptionError(message, details)

@JSExportTopLevel("Universa.WalletIDError")
class WalletIDError(
                     override val message: String,
                     override val details: HashMap[String, Any] = HashMap[String, Any]()
                   ) extends EncryptionError(message, details)

@JSExportTopLevel("Universa.DefinitionChangeError")
class DefinitionChangeError(
                             override val message: String,
                             override val details: HashMap[String, Any] = HashMap[String, Any]()
                           ) extends EncryptionError(message, details)


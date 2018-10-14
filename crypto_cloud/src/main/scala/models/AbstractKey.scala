package models

import scala.collection.mutable
import scala.scalajs.js

/** Trait for all keys */
trait AbstractKey {
  def info(): KeyInfo
  def pack(): Seq[Byte]
  def decrypt(data: Seq[Byte]): Seq[Byte]
  def encrypt(data: Seq[Byte]): Seq[Byte]

  def toRecord(): mutable.HashMap[String, Any] = {
    mutable.HashMap(
      ("keyInfo", info().pack),
      ("data", pack())
    )
  }

  def bits: Int
  def length: Int = bits / 8
}

trait AbstractKeyJS extends js.Object {
  def getAbstractKey(): AbstractKey
}
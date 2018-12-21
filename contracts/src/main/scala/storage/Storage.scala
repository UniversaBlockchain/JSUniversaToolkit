package storage

import boss.jsany._
import boss.{Boss, BossSerializable}
import models.{ContractBinary, ContractIds}
import org.scalajs.dom.window.localStorage
import tools.universa.UniversaTools._
import xchange.LocalTimeDifference

import scala.collection.mutable
import scala.reflect._
import scala.scalajs.js.annotation._

@JSExportTopLevel("LocalStorage")
class Storage[A <: BossSerializable : ClassTag]() {
  private val globalPrefix = "Universa"
  private val classPrefix = classTag[A].runtimeClass.getSimpleName
  private val instancePrefix = s"$globalPrefix-$classPrefix-objects-"

  val listKey = s"$globalPrefix-$classPrefix-ids"
  var list = getList

  private val cache: mutable.Map[String, A] = mutable.HashMap.empty[String, A]

  private def getKey(id: String, isPrefixed: Boolean): String =
    if (isPrefixed) id else instancePrefix + id

  private def encode(instance: A): String = encode64(Boss.dump(instance))
  private def decode(encoded: String): A =
    Boss.load(decode64(encoded)).asInstanceOf[A]

  private def updateList: Unit = {
    val listBuffer = mutable.ListBuffer.empty ++= list.toList
    localStorage.setItem(listKey, encode64(Boss.dump(listBuffer)))
  }

  def set(id: String, instance: A, isPrefixed: Boolean = false): Unit = {
    val key = getKey(id, isPrefixed)

    localStorage.setItem(key, encode(instance))
    cache += ((id, instance))
    list += id
    updateList
  }

  def get(id: String, isPrefixed: Boolean = false): Option[A] = {
    val key = getKey(id, isPrefixed)

    cache.get(id).orElse {
      val encodedOpt = Option(localStorage.getItem(key))

      encodedOpt.map { encoded =>
        val instance = decode(encoded)
        cache += ((key, instance))
        instance
      }
    }
  }

  def remove(id: String, isPrefixed: Boolean = false): Unit = {
    val key = getKey(id, isPrefixed)

    localStorage.removeItem(key)
    cache -= id
    list -= id
    updateList
  }

  def filter(predicate: A => Boolean): List[A] = {
    val foundInCache = cache.values.filter(predicate).toList
    if (foundInCache.nonEmpty) foundInCache
    else getAll.filter(predicate)
  }

  def find(predicate: A => Boolean): Option[A] = {
    val foundInCache = cache.values.find(predicate)
    if (foundInCache.isDefined) foundInCache
    else getAll.find(predicate)
  }

  def getList: mutable.Set[String] = {
    val encoded = localStorage.getItem(listKey)

    if (encoded == null) return mutable.Set[String]()

    val decoded = Boss.load(decode64(encoded))
    val set = mutable.Set.empty ++= decoded.asInstanceOf[mutable.ListBuffer[String]]
    set
  }

  def getAll: List[A] = list.flatMap(id => get(id)).toList
  def getFirst: Option[A] = list.headOption.flatMap(id => get(id))

  def clear = {
    localStorage.clear()
    cache.clear()
  }
}

object Storage {
  val pairStorage = new Storage[models.Pair]
  val orderStorage = new Storage[xchange.Order]
  val timeStorage = new Storage[LocalTimeDifference]()

  val contractBinaryStorage = new Storage[ContractBinary]()
  val contractIdsStorage = new Storage[ContractIds]()
}
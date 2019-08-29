package models

import boss._
import boss.jsany._

import tools.universa.UniversaTools._

import scala.scalajs.js

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.scalajs.js.annotation.{ JSExportStatic, JSExportTopLevel }

object Compound {
  val TYPE = "universa_compound"
  val VERSION = 1

  def getContract(compound: TransactionPack, tag: String): TransactionPack = {
    val info = compound.mainCapsule
      .getDefinition(s"data.contracts.$tag")
      .asInstanceOf[HashMap[String, Any]]

    val id = HashId.fromBase64(info("id").asInstanceOf[String])
    val capsule = compound.getSubItem(id)

    if (capsule.isEmpty) throw new Exception(s"No capsule found for tag '$tag'")

    val mainCapsule = capsule.get
    val tpack = new TransactionPack(mainCapsule)
    val refs = info("refs").asInstanceOf[ListBuffer[String]]

    refs.map(HashId.fromBase64(_)).map(id =>
      tpack.addReferencedItem(compound.getReferencedItem(id).get)
    )

    mainCapsule.newIds.map(id => {
      println("add new")
      tpack.addSubItem(compound.getSubItem(id).get)
    })

    mainCapsule.revokingIds.map(id => {
      println("add revoking")
      tpack.addSubItem(compound.getSubItem(id).get)
    })

    tpack
  }

  def getData(compound: TransactionPack, tag: String): HashMap[String, Any] = {
    val info = compound.mainCapsule
      .getDefinition(s"data.contracts.$tag")
      .asInstanceOf[HashMap[String, Any]]

    info("data").asInstanceOf[HashMap[String, Any]]
  }

  def isCompound(tpack: TransactionPack): Boolean = {
    val tpe = tpack.mainCapsule.getDefinition("data.type").asInstanceOf[String]
    tpe == TYPE
  }
}

object CompoundExp {
  @JSExportTopLevel("Universa.Compound.getData")
  def getData(
    compound: TransactionPackExported,
    tag: String
  ): js.Dictionary[js.Any] = {
    Boss.write(Compound.getData(compound.tp, tag)).asInstanceOf[js.Dictionary[js.Any]]
  }

  @JSExportTopLevel("Universa.Compound.isCompound")
  def isCompound(tpack: TransactionPackExported): Boolean =
    Compound.isCompound(tpack.tp)

  @JSExportTopLevel("Universa.Compound.getContract")
  def getContract(
    compound: TransactionPackExported,
    tag: String
  ): TransactionPackExported = {
    new TransactionPackExported(Compound.getContract(compound.tp, tag))
  }
}

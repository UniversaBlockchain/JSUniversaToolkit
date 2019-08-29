package models.contracts

import boss._
import boss.jsany._
import cloud.{ Api, CryptoCloud }
import files.{ Archive, ArchiveExp, FileBoss, FileInfo, FileInfoExp }
import models._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{ JSExportStatic, JSExportTopLevel }
import scala.util.{ Failure, Success }

import tools.universa.UniversaTools._

@JSExportTopLevel("Universa.NotaryContract")
class NotaryContractExported(
  override val contract: NotaryContract
) extends ContractExported(contract) {
  override def templateName = ContractType.NOTARY_CONTRACT.toString

  def addFiles(
    newFilesExp: js.Array[FileInfoExp],
    attachmentId: Double
  ): Unit = {
    val newFiles = newFilesExp.map { file => file.fi }
    val existingMeta = getFilesMeta() // Seq[FileBoss]

    val newMeta = newFiles.map { file => FileBoss(file) }.toSeq
    //we will overwrite files with the same name
    val existingUniqueMeta = existingMeta.filterNot(ex =>
      newMeta.exists(a => a.fileName == ex.fileName)
    )

    val resultMeta = (existingUniqueMeta ++ newMeta).to[ListBuffer]
    temp.capsule.setDefinition("data.files", Boss.dump(resultMeta))
    contract.attachmentId = Some(attachmentId)
  }

  def setHolderIdentifier(identifier: String): Unit = {
    temp.setDefinition("data.holder_identifier", identifier)
    temp.lockData
  }

  def getHolderIdentifier(): String =
    temp.getDefinition("data.holder_identifier").asInstanceOf[String]

  // def removeFiles(idsToRemove: js.Array[js.Array[Byte]], api: Api): js.Promise[Double] = {
  //   val existingMeta = getFilesMeta()
  //   val ids64 = idsToRemove.map { id => encode64(id) }
  //   val leftFiles = existingMeta.filterNot(ex => ids64.contains(ex.hashId.base64))

  //   if (leftFiles.length == existingMeta.length)
  //     return Future.failed(new RuntimeException("No files to remove exist in contract")).toJSPromise

  //   temp.capsule.setDefinition("data.files", Boss.dump(leftFiles.to[ListBuffer]))

  //   contract.attachmentId
  //     .map(id => ArchiveExp.downloadFromCloud(id, api, true).map(_.files))
  //     .getOrElse(Future.successful(js.Array[FileInfo]()))
  //     .map { existingFiles =>
  //       existingFiles.asInstanceOf[js.Array[FileInfo]].filter(fileInfo =>
  //         leftFiles.map(_.fileName).contains(fileInfo.name)
  //       )
  //     }
  //     .flatMap(filesToSave => ArchiveExp.uploadToCloud(Archive(filesToSave), api))
  //     .andThen {
  //       case Failure(t) => println(t)
  //       case Success(itemId) =>
  //         contract.attachmentId = Some(itemId)
  //     }
  //     .toJSPromise
  // }

  def checkFilesInContract(filesExp: js.Array[FileInfoExp]): Boolean = {
    val files = filesExp.map(_.fi)
    val hashes: Seq[FileBoss] = getFilesMeta()

    if (hashes.length != files.length) return false
    hashes.forall(fh =>
      files.exists(fi => fi.name == fh.fileName && fi.hashId.composite3 == fh.hashId.composite3)
    )
  }

  private def getFilesMeta(): Seq[FileBoss] = {
    Option(temp.capsule.getDefinition("data.files")).map { str =>
      val bytes = str.asInstanceOf[Seq[Byte]]
      Boss.load(bytes).asInstanceOf[Seq[FileBoss]]
    }.getOrElse(Seq.empty[FileBoss])
  }
}

class NotaryContract(originalPacked: Seq[Byte],
                     tempPacked: Option[Seq[Byte]] = None,
                     pendingPacked: Option[Seq[Byte]] = None)
  extends Contract(originalPacked, tempPacked, pendingPacked) with ContractTemplate {

  override def toExport[T >: ContractExported](): T = new NotaryContractExported(this)
}

object NotaryContract {
  def apply(cap: Capsule): NotaryContract = new NotaryContract(cap.currentBinary)
}

object NotaryContractExported {

  @JSExportStatic
  def create(key: PublicKeyExported, useLongAddress: Boolean): NotaryContractExported = {
    val cap = CapsuleExported.createByKey(key, useLongAddress)
    cap.contract.setDefinitionData("template_name", ContractType.NOTARY_CONTRACT.toString)
    cap.lockDataAndSign()
    val contract = NotaryContract(cap)
    new NotaryContractExported(contract)
  }

  @JSExportStatic
  def create(address: String): NotaryContractExported = {
    val cap = CapsuleExported.createByAddress(decode58(address))
    cap.contract.setDefinitionData("template_name", ContractType.NOTARY_CONTRACT.toString)
    cap.lockDataAndSign()
    val contract = NotaryContract(cap)
    new NotaryContractExported(contract)
  }
}

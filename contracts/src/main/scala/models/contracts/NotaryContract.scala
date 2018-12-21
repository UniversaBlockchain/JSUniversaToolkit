package models.contracts

import boss._
import boss.jsany._
import cloud.Api
import files.{Archive, ArchiveExported, FileBoss, FileInfo}
import models._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.util.{Failure, Success}

@JSExportTopLevel("Universa.NotaryContract")
class NotaryContractExported(override val contract: NotaryContract) extends ContractExported(contract) {
  override def templateName = ContractType.NOTARY_CONTRACT.toString

  def addFiles(newFiles: js.Array[FileInfo], api: Api): js.Promise[Double] = {
    val existingFiles = getExistingFilesInContract()

    val newHashes = newFiles.map { file => FileBoss(file) }.toSeq
    //we will overwrite files with the same name
    val existingUniqueFiles = existingFiles.filterNot(ex => newHashes.exists(a => a.fileName == ex.fileName))

    val fileBosses = existingUniqueFiles ++ newHashes
    temp.capsule.setDefinition("data.files", Boss.dump(fileBosses.to[ListBuffer]))

    val existingFilesFuture = contract.filesCloudId
      .map(id => ArchiveExported.downloadFromCloud(id, api, true).map(_.fileInfos))
      .getOrElse(Future.successful(js.Array[FileInfo]()))

    existingFilesFuture
      .map{existingFiles => existingFiles.filter(fileInfo => existingUniqueFiles.exists(_.fileName == fileInfo.name))}
      .map(leftFiles => leftFiles ++ newFiles)
      .flatMap(filesToSave => ArchiveExported.uploadToCloud(Archive(filesToSave), api))
      .andThen {
        case Failure(t) => println(t)
        case Success(itemId) =>
          //save it to contract and storage
          contract.filesCloudId = Some(itemId)
          contract.saveOrUpdateContractIds()
      }
      .toJSPromise
  }

  def removeFiles(filesNamesToDelete: js.Array[String], api: Api): js.Promise[Double] = {
    if (filesNamesToDelete.isEmpty) {
      return Future.successful(null).toJSPromise
    }

    val existingFiles = getExistingFilesInContract()
    val leftFiles = existingFiles.filterNot(ex => filesNamesToDelete.contains(ex.fileName))
    if (leftFiles.length == existingFiles.length)
      return Future.failed(new RuntimeException("No files to remove exist in contract")).toJSPromise

    temp.capsule.setDefinition("data.files", Boss.dump(leftFiles.to[ListBuffer]))

    contract.filesCloudId
      .map(id => ArchiveExported.downloadFromCloud(id, api, true).map(_.fileInfos))
      .getOrElse(Future.successful(js.Array[FileInfo]()))
      .map{existingFiles => existingFiles.filter(fileInfo => leftFiles.map(_.fileName).contains(fileInfo.name))}
      .flatMap(filesToSave => ArchiveExported.uploadToCloud(Archive(filesToSave), api))
      .andThen {
        case Failure(t) => println(t)
        case Success(itemId) =>
          //save it to contract and storage
          contract.filesCloudId = Some(itemId)
          contract.saveOrUpdateContractIds()

      }
      .toJSPromise
  }

  def checkFilesInContract(files: js.Array[FileInfo]): Boolean = {
    val hashes: Seq[FileBoss] = getExistingFilesInContract()

    if (hashes.length != files.length) return false
    hashes.forall(fh =>
      files.exists(fi => fi.name == fh.fileName && fi.hashId.composite3 == fh.hashId.composite3)
    )
  }

  private def getExistingFilesInContract(): Seq[FileBoss] = {
    Option(temp.capsule.getDefinition("data.files"))
      .map{str =>
        val bytes = str.asInstanceOf[Seq[Byte]]
        Boss.load(bytes).asInstanceOf[Seq[FileBoss]]
      }
      .getOrElse(Seq.empty[FileBoss])
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
    val contract = NotaryContract(cap)
    new NotaryContractExported(contract)
  }
}
package files

import cloud.{Api, Item}
import models.{CloudElement, CloudFunc, HashId}
import tools.JSZip

import scala.collection._
import scala.collection.mutable.{HashMap, Map}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExportAll, JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.{JSON, WrappedArray}
import scala.util.{Failure, Success}

@JSExportTopLevel("FileInfo")
@JSExportAll
case class FileInfo(content: js.Array[Byte], name: String, description: String = null) {
  private var hashIdOpt: Option[HashId] = None
  def withHashId(hashId: HashId): FileInfo = {
    hashIdOpt = Some(hashId)
    this
  }

  val extension: String = name.split("\\.").last
  def hashId: HashId = hashIdOpt.getOrElse(HashId(content))
}

case class Archive(fileInfos: js.Array[FileInfo])

@JSExportTopLevel("Universa.Archive")
class ArchiveExported(val archive: Archive) extends CloudElement {
  def ToCloud(api: Api): js.Promise[Double] = {
    ArchiveExported.uploadToCloudJS(this, api)
  }

  def fileInfos: js.Array[FileInfo] = archive.fileInfos
}

object ArchiveExported extends CloudFunc[ArchiveExported] {

  private val BatchSize = 10000 //bytes
  val ItemTag: String = "archiveTag"

  @JSExportStatic("FromFiles")
  def apply(fileInfos: js.Array[FileInfo]): ArchiveExported = {
    new ArchiveExported(Archive(fileInfos))
  }

  @JSExportStatic("ToCloud")
  def uploadToCloudJS(filesObject: ArchiveExported, api: Api): js.Promise[Double] = {
    uploadToCloud(filesObject.archive, api).toJSPromise
  }

  def uploadToCloud(archive: Archive, api: Api): Future[Double] = {
    type StoreType = js.Array[Byte]
    val files = archive.fileInfos

    def zipFiles: Future[js.Object] = {
      var zip = new JSZip()
      files.foreach { eachFile =>
        zip.file(eachFile.name, eachFile.content, Map[String, Any]("binary" -> true).toJSDictionary)
      }
      val options = HashMap[String, Any](
        "type" -> "array",
        "compression" -> "DEFLATE",
        "compressionOptions" -> Map("level" -> 9).toJSDictionary
      )
      zip.generateAsync(options.toJSDictionary).toFuture
    }

    def saveAllToCloud(zipped: js.Object): Future[Seq[Int]] = {
      val batches = zipped.asInstanceOf[StoreType].grouped(BatchSize).toSeq
      val ids = batches.map { singleBatch =>
        var item: Item = null
        item = new Item(api)
        item.setTag(ItemTag)
        item.setPrivateData("fileContent", singleBatch.toBuffer)
        item.save.map(_ => item.id.get)
      }
      val composeFuture = Future.sequence(ids)
      composeFuture
    }

    def cookMetaJson(itemIds: Seq[Int]) = {
      val fileInfos = files.map(fileInfo => {
        val info = new FileMetaInfo()
        info.fileName = fileInfo.name
        info.hashId = fileInfo.hashId.composite3.toJSArray
        info.description = fileInfo.description
        info
      })
      val tailItemIds = itemIds.tail
      val metaInfo = new ArchiveMetaInfo()
      metaInfo.fileInfos = fileInfos
      metaInfo.itemIds = tailItemIds.map(_.toDouble).toJSArray
      metaInfo
    }

    def saveMetaInfoToHeadItem(itemIds: Seq[Int]): Future[Double] = {
      val headCloudId = itemIds.head
      val loadedItemFuture: Future[Item] =
        api.getItem(api => new Item(api), HashMap[String, Any]("id" -> headCloudId))
      loadedItemFuture.flatMap { headItem =>
        val metaInfo: ArchiveMetaInfo = cookMetaJson(itemIds)
        headItem.setPrivateData("archiveMetaInfo", JSON.stringify(metaInfo))
        headItem.save.map { _ =>
          headItem.id.get.toDouble
        }
      }
    }

    val headItemIdFuture = for {
      content <- zipFiles
      itemIds <- saveAllToCloud(content)
      headItemId <- saveMetaInfoToHeadItem(itemIds)
    } yield headItemId

    headItemIdFuture
  }

  @JSExportStatic("FromCloud")
  def downloadFromCloudJS(headItemId: Double, api: Api): js.Promise[ArchiveExported] = {
    downloadFromCloud(headItemId, api, false)
      .map(archive => new ArchiveExported(archive))
      .toJSPromise
  }

  def downloadFromCloud(headItemId: Double, api: Api, destroyItemsAfterDownload: Boolean): Future[Archive] = {
    type StoreType = WrappedArray[Byte]

    def getHeadItem: Future[Item] = {
      api
        .getItem(api => new Item(api), HashMap[String, Any]("id" -> headItemId.toInt))
        .andThen{
          case Success(item) => if (destroyItemsAfterDownload) item.destroy
          case _ =>
        }
    }

    def composeFullContent(tailIds: Seq[Double], headContent: StoreType): Future[StoreType] = {
      if (tailIds.nonEmpty) {
        val items = tailIds.map { id => api.getItem(api => new Item(api), HashMap[String, Any]("id" -> id.toInt)) }

        for {
          itemSeq <- Future.sequence(items)
          allFilesBytes = {
            val contentParts = itemSeq.map {
              _.priv("fileContent").asInstanceOf[StoreType]
            }.fold(WrappedArray[Byte]())(_ ++ _)
            headContent ++ contentParts
          }
          _ = if (destroyItemsAfterDownload) itemSeq.foreach(_.destroy)
        } yield allFilesBytes
      } else {
        Future.successful(headContent)
      }
    }

    def getAllFiles(filemetas: Seq[FileMetaInfo], zipContent: StoreType): Future[js.Array[FileInfo]] = {
      var zip = new JSZip()
      val futureResult = zip.loadAsync(zipContent, Map[String, Any]("base64" -> false).toJSDictionary).toFuture
      futureResult.flatMap { resultZipObject =>
        val resultZip = resultZipObject.asInstanceOf[JSZip]
        val files = filemetas.map { metaInfo =>
          resultZip.file(metaInfo.fileName).async("array").toFuture
            .map(content =>
              FileInfo(content.asInstanceOf[js.Array[Byte]], metaInfo.fileName, metaInfo.description)
                .withHashId(HashId.fromBytes(metaInfo.hashId))
            )
        }
        Future.sequence(files).map(fileInfos => js.Array(fileInfos: _*))
      }
    }

    val archiveFuture = for {
      headItem <- getHeadItem
      archiveMetaJson = headItem.priv("archiveMetaInfo").asInstanceOf[String]
      headContent = headItem.priv("fileContent").asInstanceOf[StoreType]
      archiveMeta = JSON.parse(archiveMetaJson).asInstanceOf[ArchiveMetaInfo]
      zippedContent <- composeFullContent(archiveMeta.itemIds, headContent)
      files <- getAllFiles(archiveMeta.fileInfos, zippedContent)
    } yield {
      Archive(files)
    }

    archiveFuture
  }

}

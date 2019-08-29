package files

import boss._
import boss.jsany._

import cloud.{ Api, Item, CryptoCloud }
import models.{ CloudElement, CloudFunc, HashId, HashIdExported }
import tools.JSZip
import tools.universa.UniversaTools._
import tools.universa.ImplicitConverters._

import scala.collection._
import scala.collection.mutable.{ HashMap, Map, ListBuffer }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{ JSExportAll, JSExportStatic, JSExportTopLevel }
import scala.scalajs.js.{ JSON, WrappedArray }
import scala.util.{ Failure, Success }

case class FileInfo(
  content: Seq[Byte],
  name: String,
  description: String = null
) {
  private var hashIdOpt: Option[HashId] = None

  val extension: String = name.split("\\.").last
  def setHashId(hashId: HashId): Unit = hashIdOpt = Some(hashId)

  def hashId: HashId = {
    hashIdOpt match {
      case Some(value) => value
      case None => {
        val id = HashId(content)
        hashIdOpt = Some(id)
        id
      }
    }
  }

  def pack: js.Dictionary[Any] = js.Dictionary(
    ("content", encode64(content)),
    ("name", name),
    ("description", description)
  )
}

object FileInfo {
  def unpack(packed: js.Dictionary[Any]): FileInfo = {
    new FileInfo(
      decode64(packed("content").asInstanceOf[String]),
      packed("name").asInstanceOf[String],
      packed("description").asInstanceOf[String]
    )
  }
}

case class Archive(files: Seq[FileInfo]) {
  def pack: js.Dictionary[Any] = js.Dictionary(
    "files" -> files.map(_.pack).toJSArray
  )

  def upload(api: Api): Future[Double] = Archive.upload(this, api)

  def packZip: Future[Seq[Byte]] = {
    var zip = new JSZip()

    files.foreach { file =>
      zip.file(
        file.name,
        file.content.toJSArray,
        js.Dictionary("binary" -> true)
      )
    }

    zip.generateAsync(Archive.zipOptions).toFuture.map(_.asInstanceOf[Seq[Byte]])
  }
}

object Archive {
  type StoreType = WrappedArray[Byte]

  val batchSize = 1024 * 512 // 512kb per batch
  val tag: String = "zip_archive"
  val zipOptions = js.Dictionary[Any](
    "type" -> "array",
    "compression" -> "DEFLATE",
    "compressionOptions" -> js.Dictionary("level" -> 9)
  )

  def unpack(packed: js.Dictionary[Any]): Archive = {
    val files = packed("files").asInstanceOf[js.Array[js.Dictionary[Any]]]
    new Archive(files.map(FileInfo.unpack(_)))
  }

  def downloadParts(headerId: Double, api: Api): Future[ListBuffer[Item]] = {
    val parts = ListBuffer[Item]()

    def loadPart(id: Double): Future[Item] = api.getItem(
      api => new Item(api),
      HashMap(("id" -> id.toInt))
    )

    for {
      header <- loadPart(headerId)
      archiveMetaJson = header.priv("archive_meta").asInstanceOf[String]
      archiveMeta = JSON.parse(archiveMetaJson).asInstanceOf[ArchiveMetaInfo]
      extraIds = archiveMeta.itemIds.toSeq
      extraParts <- Future.sequence(extraIds.map(loadPart(_)))
    } yield {
      parts += header
      parts ++= extraParts
      parts
    }
  }

  def destroy(headerId: Double, api: Api): Future[ListBuffer[Boolean]] = {
    for {
      parts <- downloadParts(headerId, api)
      result <- Future.sequence(parts.map(_.destroy))
    } yield result
  }

  def downloadZip(headerId: Double, api: Api): Future[StoreType] = {
    for {
      parts <- downloadParts(headerId, api)
      archiveBin = parts.map {
        _.priv("content").asInstanceOf[StoreType]
      }.fold(WrappedArray[Byte]())(_ ++ _)
    } yield archiveBin
  }

  def download(headerId: Double, api: Api): Future[Archive] = {
    val zip = new JSZip()

    def extract(
      filesMeta: Seq[FileMetaInfo],
      zipContent: StoreType
    ): Future[Seq[FileInfo]] = {
      var zip = new JSZip()

      val zipLoader = zip.loadAsync(
        zipContent,
        js.Dictionary("base64" -> false)
      ).toFuture

      zipLoader.flatMap { zipArchive =>
        val zip = zipArchive.asInstanceOf[JSZip]
        val fileLoaders = filesMeta.map { metaInfo =>
          zip.file(metaInfo.fileName).async("array").toFuture.map(content => {
            val fi = FileInfo(
              content.asInstanceOf[js.Array[Byte]].toSeq,
              metaInfo.fileName,
              metaInfo.description
            )
            // Don't calculate again, take from meta info
            fi.setHashId(HashId.fromBytes(decode64(metaInfo.hashId)))
            fi
          })
        }

        Future.sequence(fileLoaders).map(files => Seq(files: _*))
      }
    }

    for {
      header <- api.getItem(api =>
        new Item(api), HashMap(("id" -> headerId.toInt)))
      archiveMetaJson = header.priv("archive_meta").asInstanceOf[String]
      archiveMeta = JSON.parse(archiveMetaJson).asInstanceOf[ArchiveMetaInfo]
      zipEncoded <- downloadZip(headerId, api)
      files <- extract(archiveMeta.filesMeta, zipEncoded)
    } yield {
      Archive(files)
    }
  }

  def upload(archive: Archive, api: Api): Future[Double] = {
    type StoreType = js.Array[Byte]

    val files = archive.files

    def packToZip: Future[js.Object] = {
      var zip = new JSZip()

      files.foreach { file =>
        zip.file(
          file.name,
          file.content.toJSArray,
          js.Dictionary("binary" -> true)
        )
      }

      zip.generateAsync(Archive.zipOptions).toFuture
    }

    def uploadParts(encoded: js.Object): Future[Seq[Int]] = {
      val parts = encoded.asInstanceOf[StoreType].grouped(Archive.batchSize).toSeq

      val ids = parts.map { singleBatch =>
        var item: Item = null
        item = new Item(api)
        item.setTag(Archive.tag)
        item.setPrivateData("content", singleBatch.toBuffer)
        item.save.map(_ => item.id.get)
      }

      Future.sequence(ids)
    }

    def getMeta(itemIds: Seq[Int]) = {
      val filesMeta = files.map(file => {
        val info = new FileMetaInfo()
        info.fileName = file.name
        info.hashId = file.hashId.base64
        info.description = file.description
        info
      })

      val tailItemIds = itemIds.tail
      val archiveMeta = new ArchiveMetaInfo()
      archiveMeta.filesMeta = filesMeta.toJSArray
      archiveMeta.itemIds = tailItemIds.map(_.toDouble).toJSArray
      archiveMeta
    }

    def saveMeta(itemIds: Seq[Int]): Future[Double] = {
      val headId = itemIds.head
      val downloadHeader: Future[Item] =
        api.getItem(api => new Item(api), HashMap(("id" -> headId)))

      downloadHeader.flatMap { header =>
        val metaInfo: ArchiveMetaInfo = getMeta(itemIds)
        header.setPrivateData("archive_meta", JSON.stringify(metaInfo))
        header.save.map { _ => headId }
      }
    }

    val headerIdFuture = for {
      zipped <- packToZip
      partIds <- uploadParts(zipped)
      headerId <- saveMeta(partIds)
    } yield headerId

    headerIdFuture
  }
}

@JSExportTopLevel("FileInfo")
class FileInfoExp(val fi: FileInfo) extends js.Object {
  val extension: String = fi.extension
  val name: String = fi.name
  val content: js.Array[Byte] = ensureBytes(fi.content).toJSArray

  def hashId: HashIdExported = new HashIdExported(fi.hashId)
  def description: String = fi.description

  def pack(): js.Dictionary[_] = fi.pack
}

object FileInfoExp {
  @JSExportStatic
  def create(
    content: js.Array[Byte],
    name: String,
    description: String
  ): FileInfoExp =
    new FileInfoExp(FileInfo(content.toSeq, name, description))

  @JSExportStatic
  def create(content: js.Array[Byte], name: String): FileInfoExp =
    new FileInfoExp(FileInfo(content, name))

  def unpack(packed: js.Dictionary[Any]): FileInfoExp = {
    // val packedMap = Boss.read[js.Any](packed).asInstanceOf[js.Dictionary[Any]]
    new FileInfoExp(FileInfo.unpack(packed))
  }
}

@JSExportTopLevel("Universa.Archive")
class ArchiveExp(val archive: Archive) extends CloudElement {
  def upload(api: Api): js.Promise[Double] =
    Archive.upload(archive, api).toJSPromise

  def files: js.Array[FileInfoExp] =
    archive.files.map(new FileInfoExp(_)).toJSArray

  def pack(): js.Dictionary[_] = archive.pack

  def packZip(): js.Promise[js.Array[Byte]] =
    archive.packZip.map(_.toJSArray).toJSPromise
}

object ArchiveExp extends CloudFunc[ArchiveExp] {
  val ItemTag = Archive.tag

  def unpack(packed: js.Dictionary[js.Any]): ArchiveExp = {
    val filesPacked = packed("files")
      .asInstanceOf[js.Array[js.Dictionary[Any]]]
    val files = filesPacked.map(fi => FileInfo.unpack(fi))
    new ArchiveExp(new Archive(files))
  }

  @JSExportStatic("fromFiles")
  def apply(files: js.Array[FileInfoExp]): ArchiveExp = {
    new ArchiveExp(Archive(files.map(_.fi)))
  }

  @JSExportStatic("upload")
  def uploadToCloudJS(archive: ArchiveExp, api: Api): js.Promise[Double] =
    Archive.upload(archive.archive, api).toJSPromise

  def uploadWorker(
    serializedApi: js.Dictionary[Any],
    filesPacked: js.Array[Any],
    callback: js.Function1[js.Any, Unit]
  ): Unit = {
    val apiLoader = CryptoCloud.deserialize(serializedApi)

    val files = filesPacked.map(packed =>
      FileInfoExp.unpack(packed.asInstanceOf[js.Dictionary[Any]])
    )
    val archive = Archive(files.map(_.fi))

    apiLoader map {
      api => archive.upload(api).onComplete {
        case Success(headerId) =>
          callback(js.Array("Double", headerId.asInstanceOf[Double]))
        case Failure(e) =>
          callback(e.getMessage) //TODO to unify with privateKey use smth like js.Dynamic(...)
      }
    }
  }

  def downloadWorker(
    serializedApi: js.Dictionary[Any],
    headerId: Double,
    callback: js.Function1[js.Any, Unit]
  ): Unit = {
    val apiLoader = CryptoCloud.deserialize(serializedApi)

    apiLoader map {
      api => Archive.download(headerId, api).onComplete {
        case Success(archive) =>
          val resultArray = js.Array("Archive", new ArchiveExp(archive).pack)
          callback(resultArray)
        case Failure(exception) =>
          callback(exception.getMessage) //TODO to unify with privateKey use smth like js.Dynamic(...)
      }
    }
  }

  def downloadZipWorker(
    serializedApi: js.Dictionary[Any],
    headerId: Double,
    callback: js.Function1[js.Any, Unit]
  ): Unit = {
    val apiLoader = CryptoCloud.deserialize(serializedApi)

    apiLoader map {
      api => Archive.downloadZip(headerId, api).onComplete {
        case Success(archive) => {
          val resultArray = js.Array("Bytes", archive.toJSArray)
          callback(resultArray)
        }
        case Failure(exception) =>
          callback(exception.getMessage) //TODO to unify with privateKey use smth like js.Dynamic(...)
      }
    }
  }

  @JSExportStatic("download")
  def downloadFromCloudJS(
    headerId: Double,
    api: Api
  ): js.Promise[ArchiveExp] = {
    Archive.download(headerId, api).map(archive =>
      new ArchiveExp(archive)
    ).toJSPromise
  }
}

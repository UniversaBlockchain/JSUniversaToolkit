package cloud

import tools.universa.SHA
import tools.universa.UniversaTools._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.Promise
import scala.scalajs.js.annotation.JSExportTopLevel


object Tools {
  /** Calculates some digest from string
    *
    * @param source - some string
    */
  def syntex1(source: String): Seq[Byte] = {
    val sourceLen = source.length()
    val data = s"$source$sourceLen"

    val s = asByteString(new SHA(256).get(data))
    val crc = crc32(data + s)

    asByteSeq(s + asByteString(crc))
  }



  @JSExportTopLevel("CryptoCloud.loadAllItems")
  def loadAllItemsJS(
                      api: Api,
                      tagName: String,
                      batchSize: Int
                    ): Promise[js.Array[Item]] = {
    loadAllItems(api, tagName, batchSize)
      .map(_.toJSArray)
      .toJSPromise
  }

  @JSExportTopLevel("CryptoCloud.loadAllItems")
  def loadAllItemsJS(
                      api: Api,
                      options: js.Dictionary[Any]
                    ): Promise[js.Array[Item]] = {
    val tagName: String = options.get("tagName").map(_.toString)
      .getOrElse(throw new ApiError("tagName must be defined in dictionary"))
    val batchSize: Int = options.get("batchSize").map(_.asInstanceOf[Int])
      .getOrElse(throw new ApiError("batchSize must be defined in dictionary"))
    val maxCount: Int = options.get("maxCount").map(_.asInstanceOf[Int])
      .getOrElse(Int.MaxValue)

    loadAllItems(api, tagName, batchSize, maxCount)
      .map(_.toJSArray)
      .toJSPromise
  }

  private def loadAllItems(
                      api: Api,
                      tagName: String,
                      batchSize: Int,
                      maxCount: Int = Int.MaxValue
                    ): Future[ListBuffer[Item]] = {
    val tags = ListBuffer[String](tagName)

    def nextBatch(localResult: Future[ListBuffer[Item]], fullResults: ListBuffer[Item]): Future[ListBuffer[Item]] = {
      localResult
        .flatMap { batchItems =>
          val batchItemsLength = batchItems.length
          val fullResultLength = fullResults.length

          if (fullResultLength + batchItemsLength >= maxCount) {
            Future.successful(fullResults ++ batchItems.take(maxCount - fullResultLength))
          } else if (batchItemsLength == batchSize) {
            val minSerial = batchItems.flatMap(_.serial).min
            val nextItems: Future[ListBuffer[Item]] =
              api.loadItems(mutable.HashMap(
                "tags" -> tags,
                "beforeSerial" -> minSerial,
                "limit" -> batchSize
              ))
            nextBatch(nextItems, fullResults ++ batchItems)
          } else {
            Future.successful(fullResults ++ batchItems)
          }
        }
    }

    val items: Future[ListBuffer[Item]] = api.loadItems(mutable.HashMap(
      "tags" -> tags,
      "latestSerial" -> true,
      "limit" -> batchSize
    ))
    nextBatch(items, ListBuffer())
  }

  def importAllObjects[T](
                           api: Api,
                           tagName: String,
                           converter: Item => Option[T],
                           batchSize: Int = 5
                         ): Future[js.Array[T]] = {

    val items = loadAllItems(api, tagName, batchSize)
    items.map{items =>
      items.flatMap(item => converter(item)).toJSArray
    }
  }

  def importAllSerializedObjects(
    sessionToken: String,
    tagName: String
  ): Future[js.Array[Seq[Byte]]] = {

    val tags = ListBuffer[String](tagName)
    val Limit = 50

    def nextBatch(
      items2: Future[ListBuffer[(Option[Int], Seq[Byte])]],
      results: js.Array[Seq[Byte]]
    ): Future[js.Array[Seq[Byte]]] = {
      items2
        .flatMap { items =>
          val count = items.length
          val res = items.map(_._2).toJSArray
          if (count == Limit) {
            val minSerial = items.flatMap(_._1).min
            val items2 =
              CryptoCloud.loadItemsAndSerialize(sessionToken,
                HashMap(
                  "tags" -> tags,
                  "beforeSerial" -> minSerial,
                  "limit" -> Limit
                ))
            nextBatch(items2, results ++ res)
          } else {
            Future.successful(results ++ res)
          }
        }
    }

    val items: Future[ListBuffer[(Option[Int], Seq[Byte])]] = CryptoCloud.loadItemsAndSerialize(sessionToken,
      mutable.HashMap(
        "tags" -> tags,
        "latestSerial" -> true,
        "limit" -> Limit
      )
    )
    nextBatch(items, js.Array())
  }
}

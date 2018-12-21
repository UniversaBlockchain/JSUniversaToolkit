package models

import cloud.Api

import scala.scalajs.js

trait CloudElement extends js.Object

trait CloudFunc[A <: CloudElement] {
  val ItemTag: String

  def uploadToCloudJS(element: A, api: Api): js.Promise[Double]
  def downloadFromCloudJS(headItemId: Double, api: Api): js.Promise[A]
}

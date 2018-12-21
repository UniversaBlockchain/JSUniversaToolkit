package xchange

import xchange.XChangeAPI.Dict

import scala.concurrent.Future

import models.{ PrivateKey, TransactionPack }

trait AbstractOrderApi {
  def send: Future[Dict]
  def getState: Future[Dict]
  def download: Future[Dict]

  def sign(privateKey: PrivateKey): TransactionPack
  def prepare: Future[Dict]
}

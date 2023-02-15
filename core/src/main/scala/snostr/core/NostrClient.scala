package snostr.core

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait NostrClient {
  def connect(): Future[Unit]

  def disconnect(): Future[Unit]

  def authenticate(authMessage: NostrEvent): Future[Unit]

  def publish(event: NostrEvent): Future[Unit]

  def subscribe(filters: Vector[NostrFilter], subscriptionId: String): Future[String]

  def unsubscribe(subscriptionId: String): Future[Unit]

  def addRelayMessageCallback(f: NostrRelayMessage => Future[Unit]): Future[Unit]

  def addUnknownRelayMessageCallback(f: (String, Throwable) => Future[Unit]): Future[Unit]

  def relayInformation(extraHeaders: Vector[(String, String)]): Future[NostrRelayInformation]
}

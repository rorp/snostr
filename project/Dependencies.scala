import sbt._

object Dependencies {
  // common
  lazy val secp256k1KmpJniJvm = "fr.acinq.secp256k1" % "secp256k1-kmp-jni-jvm" % "0.7.0"
  lazy val bitcoinKmpJvm = "fr.acinq.bitcoin" % "bitcoin-kmp-jvm" % "0.10.0"

  lazy val json4sJackson = "org.json4s" %% "json4s-jackson" % "4.0.3"

  lazy val zioJson = "dev.zio" %% "zio-json" % "0.4.2"

  lazy val akkaHttp = "com.typesafe.akka" %% "akka-http" % "10.2.7"
  lazy val akkaHttpSocks5 = "io.github.rorp" %% "akka-http-socks5" % "10.2.7"
  lazy val akkaStreams = "com.typesafe.akka" %% "akka-stream" % "2.6.20"

  // test
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.14" % Test
  lazy val akkaTestKit = "com.typesafe.akka" %% "akka-testkit" % "2.6.20" % Test
  lazy val testContainers = "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.40.12" % Test
}

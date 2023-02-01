import Dependencies._

lazy val projectName = "snostr"

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.github.rorp"
ThisBuild / organizationName := "io.github.rorp"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rorp/snostr"),
    "scm:git@github.com:rorp/snostr.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "rorp",
    name = "rorp",
    email = "rorp@protonmail.com",
    url = url("https://github.com/rorp")
  )
)

ThisBuild / description := "A Scala Nostr toolkit."
ThisBuild / licenses := List(
  "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
)
ThisBuild / homepage := Some(url("https://github.com/rorp/snostr"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

lazy val root = (project in file("."))
  .settings(
    name := projectName,
    publishArtifact := false
  )
  .aggregate(core, codecJackson, codecZioJson, clientAkkaHttp)

lazy val core = (project in file("core"))
  .settings(
    name := s"$projectName-core",
    libraryDependencies += secp256k1KmpJniJvm,
    libraryDependencies += bitcoinKmpJvm,
    libraryDependencies += scalaTest % Test,
  )

lazy val codecJackson = (project in file("codec-jackson"))
  .settings(
    name := s"$projectName-codec-jackson",
    libraryDependencies += json4sJackson % Provided,
    libraryDependencies += scalaTest % Test,
  )
  .dependsOn(core)

lazy val codecZioJson = (project in file("codec-zio-json"))
  .settings(
    name := s"$projectName-codec-zio-json",
    libraryDependencies += zioJson % Provided,
    libraryDependencies += scalaTest % Test,
  )
  .dependsOn(core)

lazy val clientAkkaHttp = (project in file("client-akka-http"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := s"$projectName-client-akka-http",
    libraryDependencies += akkaHttp,
    libraryDependencies += akkaStreams,
    libraryDependencies += akkaHttpSocks5,
    libraryDependencies += json4sJackson % Test,
    libraryDependencies += zioJson % Test,
    libraryDependencies += akkaTestKit % Test,
    libraryDependencies += testContainers % Test,
    libraryDependencies += scalaTest % Test,
  )
  .dependsOn(core)
  .dependsOn(codecZioJson % "test->test", codecJackson % "test->test")

import Dependencies._

lazy val projectName = "snostr"

ThisBuild / version := "0.1.2-SNAPSHOT"

lazy val commonSettings: Seq[Setting[_]] = Seq(
  scalaVersion := "2.13.9",
  version := (ThisBuild / version).value,
  organization := "io.github.rorp",
  organizationName := "io.github.rorp",
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/rorp/snostr"),
      "scm:git@github.com:rorp/snostr.git"
    )
  ),
  developers := List(
    Developer(
      id = "rorp",
      name = "rorp",
      email = "rorp@protonmail.com",
      url = url("https://github.com/rorp")
    )
  ),
  description := "A minimalistic Scala Nostr toolkit.",
  licenses := List(
    "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
  ),
  homepage := Some(url("https://github.com/rorp/snostr")),
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://s01.oss.sonatype.org/"
    if (version.value.endsWith("-SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials"),
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := projectName,
    publishArtifact := false
  )
  .aggregate(core, codecJackson, codecZioJson, clientAkkaHttp)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    name := s"$projectName-core",
    description := "Snostr Core",
    libraryDependencies += secp256k1KmpJniJvm,
    libraryDependencies += bitcoinKmpJvm,
    libraryDependencies += scalaTest % Test,
  )

lazy val codecJackson = (project in file("codec-jackson"))
  .settings(commonSettings: _*)
  .settings(
    name := s"$projectName-codec-jackson",
    description := "Snostr Jackson Codecs",
    libraryDependencies += json4sJackson % Provided,
    libraryDependencies += scalaTest % Test,
  )
  .dependsOn(core)

lazy val codecZioJson = (project in file("codec-zio-json"))
  .settings(commonSettings: _*)
  .settings(
    name := s"$projectName-codec-zio-json",
    description := "Snostr ZIO-JSON Codecs",
    libraryDependencies += zioJson % Provided,
    libraryDependencies += scalaTest % Test,
  )
  .dependsOn(core)

lazy val clientAkkaHttp = (project in file("client-akka-http"))
  .settings(commonSettings: _*)
  .settings(
    name := s"$projectName-client-akka-http",
    description := "Snostr Akka HTTP client",
    libraryDependencies += akkaHttp % Provided,
    libraryDependencies += akkaStreams % Provided,
    libraryDependencies += akkaHttpSocks5,
    libraryDependencies += akkaHttp % Test,
    libraryDependencies += akkaStreams % Test,
    libraryDependencies += json4sJackson % Test,
    libraryDependencies += zioJson % Test,
    libraryDependencies += akkaTestKit % Test,
    libraryDependencies += testContainers % Test,
    libraryDependencies += scalaTest % Test,
  )
  .dependsOn(core)
  .dependsOn(codecZioJson % "test->test", codecJackson % "test->test")

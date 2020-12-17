import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.brianpritchett"
ThisBuild / organizationName := "Brian Pritchett"
ThisBuild / organizationHomepage := Some(url("https://brianpritchett.com/"))
ThisBuild / description := "A class for immutable linked lists of at least two elements."
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/pritchett/twopluslist"))

lazy val root = (project in file("."))
  .settings(
    name := "TwoPlusList",
    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.4.3" % Test,
      "org.scalameta" %% "munit-scalacheck" % "0.7.19" % Test),
    testFrameworks += new TestFramework("munit.Framework")
  )

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/pritchett/twopluslist"),
    "scm:git@github.com:pritchett/twopluslist.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "pritchett",
    name  = "Brian Pritchett",
    email = "brian@brianpritchett.com",
    url   = url("https://brianpritchett.com")
  )
)

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

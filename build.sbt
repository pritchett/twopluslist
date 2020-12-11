import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.brianpritchett"
ThisBuild / organizationName := "Brian Pritchett"

lazy val root = (project in file("."))
  .settings(
    name := "TwoPlusList",
    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.4.3" % Test, "org.scalameta" %% "munit-scalacheck" % "0.7.19" % Test),
    testFrameworks += new TestFramework("munit.Framework")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

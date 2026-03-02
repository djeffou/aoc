ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode"
  )

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.9.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

val maxTestConcurrency = java.lang.Runtime.getRuntime().availableProcessors()
Global / concurrentRestrictions += Tags.limit(Tags.Test, maxTestConcurrency)
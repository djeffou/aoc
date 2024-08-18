ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2023"
  )

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
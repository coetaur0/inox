ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(name := "inox")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

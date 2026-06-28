ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "com.tape"

lazy val root = (project in file("."))
  .settings(
    name := "tape",
    version := "0.1.0",
  )

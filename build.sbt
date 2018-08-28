import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.rymurphy12",
      scalaVersion := "2.12.6",
      version      := "2.0.0-SNAPSHOT"
    )),
    name := "fpinscala",
    libraryDependencies += scalaTest % Test
  )


scalaVersion := "2.13.12"

name := "scala-dev-mooc-2024-04"
organization := "ru.otus"
version := "1.0"

resolvers += "Akka library repository".at("https://repo.akka.io/maven")

libraryDependencies += Dependencies.scalaTest
libraryDependencies ++= Dependencies.cats
libraryDependencies ++= Dependencies.zio
libraryDependencies ++= Dependencies.zioConfig
libraryDependencies ++= Dependencies.fs2
libraryDependencies ++= Dependencies.http4s
libraryDependencies += Dependencies.zioHttp
libraryDependencies += Dependencies.liquibase
libraryDependencies += Dependencies.postgres
libraryDependencies += Dependencies.logback
libraryDependencies ++= Dependencies.quill
libraryDependencies ++= Dependencies.testContainers
libraryDependencies ++= Dependencies.circe
libraryDependencies ++= Dependencies.akkaContainers

scalacOptions += "-Ymacro-annotations"

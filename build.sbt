import sbt.Keys._

organization := "com.joragupra"

name := "functional-refactoring"

version := "1.0.0"
scalaVersion in ThisBuild := "2.12.4"
scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

val scalaUri = "0.5.4"
val scalaz = "7.2.20"
val scalacticLibrary = "3.0.4"
val scalaCheck = "1.13.4"

libraryDependencies ++= Seq(
  "io.lemonlabs" %% "scala-uri" % scalaUri,
  "org.scalaz" %% "scalaz-core" % scalaz,
  "org.scalactic" %% "scalactic" % scalacticLibrary,
  "org.scalatest" %% "scalatest" % scalacticLibrary % Test,
  "org.scalacheck" %% "scalacheck" % scalaCheck % Test
)

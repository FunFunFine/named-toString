import Dependencies._

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.ffn"

lazy val root = (project in file("."))
  .settings(
    name := "named-toString",
    libraryDependencies += munit % Test,
    scalacOptions ++= Seq(
      "-Wconf:cat=unused-imports:i",
      "-Xsource:3",
      "-language:experimental.macros",
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
      munit % Test
    )
  )

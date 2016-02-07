name := "fake-file-system"

organization := "org.gestern"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test" withSources() withJavadoc(),
  "org.rogach" %% "scallop" % "0.9.5" withSources(), // command line parser
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3"
)

initialCommands := "import org.gestern.fakefilesystem._"


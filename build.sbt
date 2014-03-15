name := "Scalearn"

organization := "Kyu"

version := "0.01"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.h2database" % "h2" % "1.3.166",
  "org.jscala" %% "jscala-macros" % "0.4-SNAPSHOT"
)

resolvers += Resolver.sonatypeRepo("snapshots")
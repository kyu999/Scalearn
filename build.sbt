name := "Scalearn"

organization := "Kyu"

version := "0.01"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.apache.spark" % "spark-core_2.10" % "0.9.0-incubating",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "com.h2database" % "h2" % "1.3.166",
  "org.jscala" %% "jscala-macros" % "0.4-SNAPSHOT",
  "org.jfree" % "jfreechart" % "1.0.17"
)

resolvers += Resolver.sonatypeRepo("snapshots")
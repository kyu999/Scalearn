name := "StatsBox"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalanlp" % "breeze_2.10" % "0.6.1",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.h2database" % "h2" % "1.3.166"
)
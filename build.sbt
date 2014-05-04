name := "Scalearn"

organization := "Kyu"

version := "0.01"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.1.4" % "test",
  "org.apache.spark" % "spark-core_2.10" % "0.9.0-incubating",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "com.h2database" % "h2" % "1.3.166",
  "org.scalanlp" % "breeze_2.10" % "0.7",
  "org.scalanlp" % "breeze-natives_2.10" % "0.7",
  "eu.fakod" % "neo4j-scala_2.10" % "0.3.0")

scalacOptions ++= Seq("-unchecked", "-deprecation")
    
resolvers += Resolver.sonatypeRepo("snapshots")

resolvers ++= Seq(
            "Neo4j Scala Repo" at "http://m2.neo4j.org/content/repositories/releases",
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/")
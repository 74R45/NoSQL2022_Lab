ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "mtDNA"
  )

libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "4.4.5"

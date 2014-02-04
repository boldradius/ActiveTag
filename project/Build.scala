import java.io.File
import sbt._
import Keys._
import sbt.File

object Build extends sbt.Build {
  lazy val all = Project(
    id = "ActiveTag",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      scalaVersion := "2.10.3",
      name := "activetag",
      organization := "co.tindr",
      version := "0.1-SNAPSHOT",
      scalaSource in Compile := baseDirectory.value / "shared"/ "main" / "scala",
      scalaSource in Test := baseDirectory.value / "test"/ "main" / "scala",
      resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
      libraryDependencies := Seq(
        "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
        "com.typesafe.play" %% "play" % "2.2.1",
        "org.scalaz" % "scalaz-core_2.10" % "7.0.0")))
}

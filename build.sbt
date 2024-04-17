ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "Collections"
  )

ThisBuild / triggeredMessage  := Watched.clearWhenTriggered


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"


testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.4"

lazy val root = (project in file("."))
  .settings(
    name := "Collections"
  )

triggeredMessage := Watched.clearWhenTriggered


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

libraryDependencies += "org.pegdown" % "pegdown" % "1.6.0" % "test"


testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oSD")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")
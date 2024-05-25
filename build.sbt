ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.4"

lazy val root = (project in file("."))
  .settings(
    name := "Collections"
  )

triggeredMessage := Watched.clearWhenTriggered

initialCommands in console := "import homegrown.collections._"

//resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"


scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-Xlint"
)

libraryDependencies ++=
  Seq(
    "org.scalatest" %%  "scalatest" %  "3.0.5" % "test", // http://www.scalatest.org/
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
//    "org.pegdown"  %    "pegdown" %  "1.6.0" % "test"  // https://github.com/sirthias/pegdown/
  )


//testOptions in Test ++=
//  Seq(
//    Tests.Argument(TestFrameworks.ScalaTest, "-oSD"),
//    Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")
//  )

addCommandAlias("testc", ";clean;coverage;test;coverageReport")

//coverageExcludedPackages := "Main"
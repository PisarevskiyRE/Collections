ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "Collections"
  )

ThisBuild / triggeredMessage  := Watched.clearWhenTriggered


//resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"


//libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
//libraryDependencies += "org.pegdown"  %   "pegdown" % "1.6.0" % "test"


libraryDependencies ++=
  Seq(
    "org.scalatest" %%  "scalatest" %  "3.2.18" % "test", // http://www.scalatest.org/
    "org.scalacheck" %% "scalacheck" % "1.16.0" % "test",
    "org.pegdown"  %    "pegdown" %  "1.6.0" % "test"  // https://github.com/sirthias/pegdown/
  )


//testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oSD")
//testOptions in Test +=  Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")

testOptions in Test ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test-reports-xml"),
  Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports-html"),
  Tests.Argument(TestFrameworks.ScalaTest, "-oDWF")
)
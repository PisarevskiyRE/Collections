ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "Collections",
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
  )

ThisBuild / triggeredMessage  := Watched.clearWhenTriggered

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:implicitConversions",
  "-language:higherKinds",
  //"-Ypartial-unification"
)

//resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"


//libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
//libraryDependencies += "org.pegdown"  %   "pegdown" % "1.6.0" % "test"


libraryDependencies ++=
  Seq(
    "org.scalatest" %%  "scalatest" %  "3.2.18" % "test", // http://www.scalatest.org/
    "org.scalacheck" %% "scalacheck" % "1.16.0" % "test",
  )



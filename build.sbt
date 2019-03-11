ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.game"

lazy val battle = (project in file("."))
  .settings(
    name := "Land of Trieath",
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"
  )


ThisBuild / scalaVersion := "2.12.7"
ThisBuild / organization := "com.game"

lazy val battle = (project in file("."))
  .settings(
    name := "Land of Trieath",
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "8.0.144-R12",
      "com.typesafe.play" %% "play-json" % "2.6.10"
    ),
    scalacOptions := Seq("-unchecked", "-deprecation")
  )


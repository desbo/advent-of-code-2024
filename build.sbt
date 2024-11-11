ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name                := "aoc-runner",
    idePackagePrefix    := Some("aoc"),
    libraryDependencies := Seq("org.typelevel" %% "cats-effect" % "3.5.5"),
    javaOptions += "-Dcats.effect.warnOnNonMainThreadDetected=false"
  )

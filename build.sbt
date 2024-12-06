ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-runner",
    libraryDependencies := Seq(
      "org.typelevel"                 %% "cats-effect" % "3.5.7",
      "com.softwaremill.sttp.client3" %% "core"        % "3.10.1",
      "com.lihaoyi"                   %% "os-lib"      % "0.11.3",
      "org.scalameta"                 %% "munit"       % "0.7.29" % Test
    ),
    javaOptions += "-Dcats.effect.warnOnNonMainThreadDetected=false"
  )

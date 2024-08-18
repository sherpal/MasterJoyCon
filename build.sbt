import indigoplugin._

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-feature",
  "-language:implicitConversions"
)

lazy val mygame =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin, SbtIndigo) // Enable the Scala.js and Indigo plugins
    .settings(                               // Standard SBT settings
      name         := "mygame",
      version      := "0.0.1",
      scalaVersion := "3.4.1",
      organization := "org.mygame"
    )
    .settings( // Indigo specific settings
      indigoOptions :=
        IndigoOptions.defaults
          .withTitle("My Game")
          .withWindowSize(720, 480),
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo"            % "0.17.0",
        "io.indigoengine" %%% "indigo-extras"     % "0.17.0",
        "io.indigoengine" %%% "indigo-json-circe" % "0.17.0"
      )
    )

addCommandAlias("runGame", ";compile;fastLinkJS;indigoRun")

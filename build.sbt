import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

import Dependencies._

////////////////////////////////////////////////////////////////////////////////

lazy val scalaV = "2.11.8"

////////////////////////////////////////////////////////////////////////////////

lazy val update = (project in file("update")).settings(
  scalaVersion := scalaV,

  libraryDependencies ++= updateDeps
)

////////////////////////////////////////////////////////////////////////////////

lazy val sync = (project in file("sync")).settings(
  scalaVersion := scalaV,

  libraryDependencies ++= updateDeps
)

////////////////////////////////////////////////////////////////////////////////

lazy val server = (project in file("server")).settings(  
  scalaVersion := scalaV,

  resolvers := Resolver.jcenterRepo +: resolvers.value,
  resolvers := ("Atlassian Releases" at "https://maven.atlassian.com/public/") +: resolvers.value,

  libraryDependencies ++= serverDeps,

  libraryDependencies ++= Seq(  
  specs2 % Test,
  cache,
  filters,
  ws
  ),

  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  pipelineStages := Seq(digest, gzip),  
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  mappings in (Compile, packageDoc) := Seq(),  

  routesGenerator := InjectedRoutesGenerator,
  routesImport += "utils.route.Binders._",
  
  defaultScalariformSettings,

  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(FormatXml, false)
    .setPreference(DoubleIndentClassDeclaration, false)
    .setPreference(DanglingCloseParenthesis, Preserve),

  excludeFilter in scalariformFormat := ((excludeFilter in scalariformFormat).value ||
    "Routes.scala" ||
    "ReverseRoutes.scala" ||
    "JavaScriptReverseRoutes.scala" ||
    "RoutesPrefix.scala")
).enablePlugins(PlayScala/*, LauncherJarPlugin*/, JavaAppPackaging).
  dependsOn(sharedJvm)

////////////////////////////////////////////////////////////////////////////////

lazy val client = (project in file("client")).settings(
  scalaVersion := scalaV,

  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "org.singlespaced" %%% "scalajs-d3" % "0.3.4",
    "com.lihaoyi" %%% "upickle" % "0.4.3"
  ),

  scalaJSUseMainModuleInitializer := true
).enablePlugins(ScalaJSPlugin, ScalaJSWeb).
  dependsOn(sharedJs)

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(
  scalaVersion := scalaV

).jsConfigure(_ enablePlugins ScalaJSWeb)

////////////////////////////////////////////////////////////////////////////////

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

////////////////////////////////////////////////////////////////////////////////

onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value

////////////////////////////////////////////////////////////////////////////////

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  //"-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xlint", // Enable recommended additional warnings.
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-nullary-override", // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
  "-Ywarn-numeric-widen" // Warn when numerics are widened.
)

////////////////////////////////////////////////////////////////////////////////

addCommandAlias("c","~compile")
addCommandAlias("r","run -Dhttp.port=9001")
addCommandAlias("u",";fullOptJS;project update")
addCommandAlias("y",";fullOptJS;project sync")
addCommandAlias("s","project server")

////////////////////////////////////////////////////////////////////////////////
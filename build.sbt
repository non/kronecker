import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val kroneckerSettings = Seq(
  organization := "org.spire-math",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("http://github.com/non/kronecker")),
  scalaVersion := "2.13.4",
  crossScalaVersions := Seq("2.12.12", "2.13.3"),
  scalacOptions ++=
    "-deprecation" ::
    "-encoding" :: "UTF-8" ::
    "-feature" ::
    "-unchecked" ::
    "-Xlint" ::
    "-Ywarn-value-discard" ::
    Nil,
  libraryDependencies ++=
    "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
    "org.typelevel" %% "spire" % "0.17.0" ::
    "org.typelevel" %% "spire-extras" % "0.17.0" ::
    "com.chuusai" %% "shapeless" % "2.3.3" ::
    "org.scalacheck" %% "scalacheck" % "1.15.0" % "test" ::
    "org.typelevel" %% "claimant" % "0.1.3" % "test" ::
    Nil)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val root = project
  .in(file("."))
  .aggregate(coreJVM, coreJS)
  .settings(name := "kronecker-root")
  .settings(kroneckerSettings: _*)
  .settings(noPublish: _*)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "kronecker-core")
  .settings(kroneckerSettings: _*)
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val refined = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("refined"))
  .dependsOn(core)
  .settings(name := "kronecker-refined")
  .settings(kroneckerSettings: _*)
  .settings(libraryDependencies += "eu.timepit" %% "refined" % "0.9.19")
lazy val refinedJVM = refined.jvm
lazy val refinedJS = refined.js

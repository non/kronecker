import ReleaseTransformations._

lazy val kroneckerSettings = Seq(
  organization := "org.spire-math",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("http://github.com/non/kronecker")),
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq(/*"2.10.6", */"2.11.12", "2.12.4"),
  scalacOptions ++=
    "-feature" ::
    "-deprecation" ::
    "-unchecked" ::
    Nil,
  libraryDependencies ++=
    "org.typelevel" %% "spire" % "0.14.1" ::
    "org.typelevel" %% "spire-extras" % "0.14.1" ::
    "com.chuusai" %% "shapeless" % "2.3.3" ::
    "org.scalacheck" %% "scalacheck" % "1.13.5" % "test" ::
    Nil,
  //scalaJSStage in Global := FastOptStage, //FIXME
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging),
  pomExtra := (
    <scm>
      <url>git@github.com:non/kronecker.git</url>
      <connection>scm:git:git@github.com:non/kronecker.git</connection>
    </scm>
    <developers>
      <developer>
        <id>non</id>
        <name>Erik Osheim</name>
        <url>http://github.com/non/</url>
      </developer>
    </developers>
  ),

  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges))

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

lazy val core = crossProject
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "kronecker-core")
  .settings(kroneckerSettings: _*)
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val refined = crossProject
  .crossType(CrossType.Pure)
  .in(file("refined"))
  .dependsOn(core)
  .settings(name := "kronecker-refined")
  .settings(kroneckerSettings: _*)
  .settings(libraryDependencies += "eu.timepit" %% "refined" % "0.9.1")
lazy val refinedJVM = refined.jvm
lazy val refinedJS = refined.js

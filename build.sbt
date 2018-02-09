name := "kronecker"

scalaVersion := "2.12.4"

libraryDependencies ++=
  "org.typelevel" %% "spire" % "0.14.1" ::
  "com.chuusai" %% "shapeless" % "2.3.3" ::
  "org.scalacheck" %% "scalacheck" % "1.13.5" ::
  Nil

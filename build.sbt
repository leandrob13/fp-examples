name := """book"""

version := "1.0"

scalaVersion := "2.12.2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scalatest"                %% "scalatest"                % "3.0.1" % "test",
  "org.typelevel"                %% "cats-core"                     % "1.0.0-RC1" withSources(),
  "org.typelevel"                %% "cats-macros"                     % "1.0.0-RC1" withSources(),
  "org.typelevel"                %% "cats-kernel"                     % "1.0.0-RC1" withSources(),
  "io.monix" %% "monix" % "3.0.0-M2"
)


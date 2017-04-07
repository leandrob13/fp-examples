name := """book"""

version := "1.0"

scalaVersion := "2.12.1"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")


libraryDependencies ++= Seq(
  "org.scalatest"                %% "scalatest"                % "3.0.1" % "test",
  "org.typelevel"                %% "cats"                     % "0.9.0" withSources (),
  "io.monix" %% "monix" % "2.2.4",
  "io.monix" %% "monix-cats" % "2.2.4"
)


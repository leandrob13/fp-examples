name := """book"""

version := "1.0"

scalaVersion := "2.12.2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")


libraryDependencies ++= Seq(
  "org.scalatest"                %% "scalatest"                % "3.0.1" % "test",
  "org.typelevel"                %% "cats"                     % "0.9.0" withSources (),
  "io.monix" %% "monix-eval" % "2.3.2",
  "io.monix" %% "monix-execution" % "2.3.2",
  "io.monix" %% "monix-types" % "2.3.2",
  "io.monix" %% "monix-cats" % "2.3.2"
)


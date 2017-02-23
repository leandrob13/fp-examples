name := """book"""

version := "1.0"

scalaVersion := "2.12.1"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scalatest"                %% "scalatest"                % "3.0.1" % "test",
  "org.typelevel"                %% "cats"                     % "0.9.0" withSources ()
)

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"


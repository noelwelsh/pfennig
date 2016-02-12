val cats = "org.spire-math" %% "cats" % "0.4.0-SNAPSHOT"
val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4" % "test"
val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.7.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(cats, scalaTest, simulacrum)

resolvers ++= Seq(Resolver.sonatypeRepo("snapshots"), Resolver.sonatypeRepo("releases"))

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

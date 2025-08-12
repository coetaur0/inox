ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

lazy val root = (project in file("."))
  .settings(
    name := "inox"
  )

// Some dependencies like `javacpp` are packaged with maven-plugin packaging
classpathTypes += "maven-plugin"

// Platform classifier for native library dependencies
lazy val platform = org.bytedeco.javacpp.Loader.getPlatform

libraryDependencies ++= Seq(
  "org.bytedeco" % "llvm-platform" % "20.1.7-1.5.12",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

// Fork a new JVM for 'run' and 'test:run'
fork := true

// Set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state =>
  "sbt:" + Project.extract(state).currentRef.project + "> "
}

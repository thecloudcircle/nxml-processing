name := "nxml.processing"

lazy val commonSettings = Seq(
  organization := "com.wisecube",
  version := "0.1"
)

scalaVersion := "2.11.12"

idePackagePrefix := Some("com.wisecube")

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "fat-jar-test",
    assemblyJarName in assembly := "nxml.processing.jar"
  ).
  enablePlugins(AssemblyPlugin)

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"

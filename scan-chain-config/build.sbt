import Dependencies._

libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.3"

assemblyJarName in assembly := "scan-chain-config.jar"

test in assembly := {} // Should there be tests?

assemblyOutputPath in assembly := file("../utils/bin/scan-chain-config.jar")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.ibm",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ScanChainConfig",
    libraryDependencies += scalaTest % Test,
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../src/main/scala/chiffre/scan"
  )

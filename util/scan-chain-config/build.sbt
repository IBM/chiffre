import Dependencies._

libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.0"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"

libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.0"

assemblyJarName in assembly := "scan-chain-config.jar"

test in assembly := {} // Should there be tests?

assemblyOutputPath in assembly := file("./utils/bin/scan-chain-config.jar")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.ibm",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ScanChainConfig",
    libraryDependencies += scalaTest % Test,
    unmanagedSourceDirectories in Compile += baseDirectory.value / "../../src/main/scala/leChiffre/scan"
  )

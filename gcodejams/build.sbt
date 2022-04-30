val dottyVersion = "2.12.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "nish-code-jams",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

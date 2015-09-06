organization  := "net.scivey"
name          := "scalisp"
version       := "0.1.0"

scalaVersion  := "2.11.6"

libraryDependencies ++= {
  Seq(
    "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test",
    "org.scala-lang.modules" %% "scala-async" % "0.9.5",
    "com.github.nscala-time" %% "nscala-time" % "2.2.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "org.slf4j"           %   "slf4j-log4j12" % "1.7.12",
    "org.slf4j"           %   "slf4j-api"     % "1.7.12"
  )
}

Revolver.settings

name := "beginner scalaz"

scalaVersion := "2.10.2"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-filter" % "0.6.8",
  "net.databinder" %% "unfiltered-jetty"  % "0.6.8",
  "net.databinder" %% "unfiltered-json4s" % "0.6.8",
  "org.json4s" %% "json4s-scalaz" % "3.2.5",
  "org.json4s" %% "json4s-native" % "3.2.5",
  "org.scalaz" %% "scalaz-core" % "7.0.2"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.2" % "test",
  "org.typelevel" %% "scalaz-specs2" % "0.1.4" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.2" % "test"
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

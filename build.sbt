name := "beginner scalaz"

scalaVersion := "2.10.0"

resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0-M8"
)
 
scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"
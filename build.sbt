name := "sr_lab7"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.3",
  "org.jgroups" % "jgroups" % "3.0.10.Final",
  "net.sandrogrzicic" %% "scalabuff-runtime" % "1.3.8"
)

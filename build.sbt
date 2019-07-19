
scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.1.0-M3",
  "com.typesafe.play" %% "play-ws-standalone-json" % "2.1.0-M3",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

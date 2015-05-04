name := "engines"

version := "0.1"

scalaVersion := "2.11.6"

description := "Toy Financial Messaging Engines"

resolvers += "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"

resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
  "org.scalaz.stream" %% "scalaz-stream" % "0.7a",
  "org.scodec" %% "scodec-core" % "1.7.0",
  "org.scodec" %% "scodec-bits" % "1.0.6",
  "org.scodec" %% "scodec-scalaz" % "1.0.0",
  "org.scodec" %% "scodec-stream" % "0.8.0",
  "org.scodec" %% "scodec-protocols" % "0.8.0" 
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-s", "1000")

scalacOptions ++=
  Seq("-encoding", "UTF-8", "-Yrecursion", "50", "-deprecation",
      "-unchecked", "-Xlint", "-feature",
      "-language:implicitConversions", "-language:higherKinds",
      "-language:existentials")

javacOptions ++=
  Seq("-Xlint:cast", "-Xlint:deprecation", "-Xlint:empty",
      "-Xlint:finally", "-Xlint:fallthrough", "-Xlint:overrides")

javaOptions ++= Seq("-XX:MaxJavaStackTraceDepth=1000000 -Dscala.color")

parallelExecution := false

javacOptions += "-Xlint"

scalacOptions ~= (so => (so filterNot Set("-unchecked", "-Xlint"))
                    ++ Seq("-Ywarn-nullary-override", "-Ywarn-inaccessible"))

seq(bintraySettings:_*)

name := "MCDE-EXTENDED"
organization := "io.github.edouardfouche"

version := "0.1.0"

scalaVersion := "2.12.8"
crossScalaVersions := Seq("2.11.8", "2.12.8")

javaOptions += "-Xmx10G"
javaOptions += "-Xms10G"

fork in run := true
scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies += "io.github.edouardfouche" %% "datagenerator" % "0.1.1"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

libraryDependencies += "org.jzy3d" % "jzy3d-api" % "1.0.0"
resolvers += "Jzy3d Maven Release Repository" at "http://maven.jzy3d.org/releases"
 
libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.1",
  "org.scalanlp" %% "breeze-natives" % "0.13.1"
)

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

// Note: from logback 1.1.5, threads do not inherit the MDC anymore
assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
test in assembly := {}

assemblyMergeStrategy in assembly ~= { old =>
{
  case PathList("META-INF", "datagenerator", xs @ _*) => MergeStrategy.first
  case x if x.startsWith("Main") => MergeStrategy.first 
  case x => old(x)
}
}

javacOptions ++= Seq("-encoding", "UTF-8")
name := "hypermake"
organization := "me.tongfei"
isSnapshot := true
version := "0.1.0"
scalaVersion := "2.13.12"
scalacOptions := Seq("-deprecation", "-release:8")

//enablePlugins(GraalVMNativeImagePlugin)
//ThisBuild/semanticdbVersion := scalafixSemanticdb.revision
Compile / mainClass := Some("hypermake.Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
  "org.typelevel" %% "cats-core" % "2.12.0",
  "dev.zio" %% "zio" % "1.0.18",
  "dev.zio" %% "zio-process" % "0.6.1",
  "io.circe" %% "circe-core" % "0.14.9",
  "io.circe" %% "circe-generic" % "0.14.9",
  "io.circe" %% "circe-parser" % "0.14.9",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "com.lihaoyi" %% "fansi" % "0.5.0",
  "com.github.pathikrit" %% "better-files" % "3.9.2",
  "org.jline" % "jline" % "3.26.2"
)

// Test dependencies
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.11.0",
  "org.typelevel" %% "discipline-scalatest" % "2.2.0",
  "org.scalatest" %% "scalatest" % "3.2.19",
  "org.scalacheck" %% "scalacheck" % "1.18.0",
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0",
  "io.chrisdavenport" %% "cats-scalacheck" % "0.3.2"
).map(_ % Test)

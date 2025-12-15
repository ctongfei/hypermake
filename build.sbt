name := "hypermake"
organization := "me.tongfei"
isSnapshot := true
version := "0.1.0"
scalaVersion := "2.13.18"
scalacOptions := Seq("-deprecation", "-release:8")

enablePlugins(GraalVMNativeImagePlugin)
// ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
Compile / mainClass := Some("hypermake.Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-collection-contrib" % "0.4.0",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "dev.zio" %% "zio" % "2.1.23",
  "dev.zio" %% "zio-managed" % "2.1.23",
  "dev.zio" %% "zio-process" % "0.7.2",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "com.lihaoyi" %% "fansi" % "0.5.1",
  "com.lihaoyi" %% "upickle" % "4.4.1",
  "com.github.pathikrit" %% "better-files" % "3.9.2",
  "org.jline" % "jline" % "3.30.6"
)

// Test dependencies
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-laws" % "2.13.0",
  "org.typelevel" %% "discipline-scalatest" % "2.3.0",
  "org.scalatest" %% "scalatest" % "3.2.19",
  "org.scalacheck" %% "scalacheck" % "1.19.0",
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0",
  "io.chrisdavenport" %% "cats-scalacheck" % "0.3.2"
).map(_ % Test)

def prop(name: String, default: String) = Option(System.getProperty(name)).getOrElse(default)

Compile / resourceDirectory := {
  if (prop("hypermake.stdlibasresource", "false") == "true")
    baseDirectory.value / "src" / "main" / "hypermake"
  else baseDirectory.value / "src" / "main" / "resources"
}

name := "forge"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "dev.zio"              %% "zio"          % "1.0.0-RC20",
  "org.typelevel"        %% "cats-core"    % "2.0.0",
  "com.lihaoyi"          %% "fastparse"    % "2.2.2",
  "com.github.pathikrit" %% "better-files" % "3.9.1"
)

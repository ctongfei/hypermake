name := "hypermake"
organization := "me.tongfei"
isSnapshot := true
version := "0.1.0"
scalaVersion := "2.13.8"
scalacOptions := Seq("-deprecation")

Compile/mainClass := Some("hypermake.Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-collection-contrib" % "0.2.2",
  "org.typelevel"           %% "cats-core"                % "2.9.0",
  "dev.zio"                 %% "zio"                      % "1.0.15",
  "dev.zio"                 %% "zio-process"              % "0.6.1",
  "com.lihaoyi"             %% "fastparse"                % "2.3.3",
  "com.lihaoyi"             %% "fansi"                    % "0.4.0",
  "com.github.pathikrit"    %% "better-files"             % "3.9.2",
  "org.jline"               %  "jline"                    % "3.22.0",
)

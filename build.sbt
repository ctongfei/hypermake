name := "hypermake"
organization := "me.tongfei"
isSnapshot := true
version := "0.1.0"
scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-collection-contrib" % "0.2.2",
  "org.typelevel"           %% "cats-core"                % "2.5.0",
  "dev.zio"                 %% "zio"                      % "1.0.7",
  "dev.zio"                 %% "zio-process"              % "0.3.0",
  "com.lihaoyi"             %% "fastparse"                % "2.2.2",
  "com.lihaoyi"             %% "fansi"                    % "0.2.10",
  "com.github.pathikrit"    %% "better-files"             % "3.9.1",
  "org.jline"               %  "jline"                    % "3.19.0",
)

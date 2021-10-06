name := "hypermake"
organization := "me.tongfei"
isSnapshot := true
version := "0.1.0"
scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-collection-contrib" % "0.2.2",
  "org.typelevel"           %% "cats-core"                % "2.6.1",
  "dev.zio"                 %% "zio"                      % "1.0.9",
  "dev.zio"                 %% "zio-process"              % "0.5.0",
  "com.lihaoyi"             %% "fastparse"                % "2.3.3",
  "com.lihaoyi"             %% "fansi"                    % "0.2.14",
  "com.github.pathikrit"    %% "better-files"             % "3.9.1",
  "org.jline"               %  "jline"                    % "3.20.0",
)

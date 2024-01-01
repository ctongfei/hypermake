name := "hypermake"
organization := "me.tongfei"
isSnapshot := true
version := "0.1.0"
scalaVersion := "2.13.12"
scalacOptions := Seq("-deprecation")

//enablePlugins(GraalVMNativeImagePlugin)
//ThisBuild/semanticdbVersion := scalafixSemanticdb.revision
Compile/mainClass := Some("hypermake.Main")

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-collection-contrib" % "0.2.2",
  "org.typelevel"           %% "cats-core"                % "2.9.0",
  "dev.zio"                 %% "zio"                      % "1.0.18",
  "dev.zio"                 %% "zio-process"              % "0.6.1",
  "io.circe"                %% "circe-core"               % "0.14.5",
  "io.circe"                %% "circe-generic"            % "0.14.5",
  "io.circe"                %% "circe-parser"             % "0.14.5",
  "com.lihaoyi"             %% "fastparse"                % "3.0.2",
  "com.lihaoyi"             %% "fansi"                    % "0.4.0",
  "com.github.pathikrit"    %% "better-files"             % "3.9.2",
  "org.jline"               %  "jline"                    % "3.23.0",
)

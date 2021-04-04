name := "forge"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.typelevel"        %% "cats-core"    % "2.5.0",
  "org.typelevel"        %% "cats-effect"  % "3.0.0",
  "com.lihaoyi"          %% "fastparse"    % "2.2.2",
  "com.lihaoyi"          %% "fansi"        % "0.2.10",
  "com.github.pathikrit" %% "better-files" % "3.9.1"
)

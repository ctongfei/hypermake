package forge.exception

import fastparse.Parsed

case class SourceNotFoundException(name: String)
  extends Exception(s"Source file $name not found.")

case class ParsingException(failure: Parsed.Failure)
  extends Exception(s"Parsing error: \n${failure.trace().longMsg}")

case class AxesAlignmentException(name: String, v1: Seq[String], v2: Seq[String])
  extends Exception(s"Axis $name does not align in two occurrences: {${v1.mkString(", ")}} != {${v2.mkString(", ")}}")

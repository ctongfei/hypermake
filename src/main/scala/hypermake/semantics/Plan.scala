package hypermake.semantics

import hypermake.core._

import scala.collection._

/**
 * Represents a collection of slices of tasks whose execution can be invoked from the command line.
 */
class Plan(val tasks: Seq[Cube[Task]])(implicit val ctx: ParsingContext)

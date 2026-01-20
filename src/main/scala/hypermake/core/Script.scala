package hypermake.core

import java.io.{File => JFile}
import scala.collection._

import better.files._
import zio._
import zio.process._

import hypermake.execution._
import hypermake.util._

/** Encapsulates a script together with its external arguments (passed in as environment variables). */
case class Script(
    script: String,
    args: Args[Value] = Args(Map()),
    nestingLevel: Int = 0
) {

  def withNewArgs(newArgs: Args[Value]) = Script(script, args ++ newArgs, nestingLevel)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(Args(newArgs.toMap.mapValuesE(Value.Pure)))

}

package hypermake.semantics

import scala.collection._
import hypermake.core._
import hypermake.semantics.Escaper.C


case class Script(script: String, suffix: String = "sh", args: Map[Name, Value] = Map()) {

  def withNewArgs(newArgs: Map[Name, Value]) = Script(script, suffix, args ++ newArgs)

  override def toString = {
    val argsString = args.view.map { case (k, v) => s"$k=${C.escape(v.value)}"}.mkString("\n")
    f"$argsString\n$script"
  }

}

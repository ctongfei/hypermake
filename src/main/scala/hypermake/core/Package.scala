package hypermake.core

import cats.implicits._
import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

import scala.collection._

class Package(
               val name: String,
               val `case`: Case,
               val inputs: Map[String, Value],
               val outputFileName: (String, Value.Pure),
               val decorators: Seq[Decorator],
               val rawScript: Script
)(implicit ctx: Context) {
  def on(env: Env): Task = new Task(
    name = s"$name@$env",
    env = env,
    `case` = `case`,
    inputs = inputs,
    inputEnvs = inputs.mapValuesE {
      case _: Value.Pure          => Env("")
      case _: Value.PackageOutput => env
    },
    outputFileNames = Map(outputFileName),
    outputEnvs = Map(outputFileName._1 -> env),
    decorators = decorators, // TODO: what if decorators refer to env-dependent values?
    rawScript = rawScript
  )

  def output: Value.PackageOutput = Value.PackageOutput(this)
}

/** A package can be realized on multiple environments, and cannot be dependent on any other task.
  */
case class PointedPackageTensor(
                                 name: String,
                                 cases: PointedCaseTensor,
                                 inputs: Map[String, PointedTensor[Value]],
                                 outputFileName: (String, PointedTensor[Value.Pure]),
                                 decorators: Seq[Decorator],
                                 rawScript: PointedTensor[Script]
)(implicit ctx: Context)
    extends PointedTensor[Package] {

  def get(c: Case): Option[Package] = {
    if (cases containsCase c) {
      Some(
        new Package(
          name = name,
          `case` = cases.normalizeCase(c),
          inputs = inputs.mapValuesE(_.select(c).default),
          outputFileName = outputFileName._1 -> outputFileName._2.select(c).default,
          decorators = decorators,
          rawScript = rawScript.select(c).default
        )
      )
    } else None
  }

  /** Returns a task that builds this package on a specific environment.
    */
  def on(env: Env)(implicit ctx: Context) = new PointedTaskTensor(
    s"$name@${env.name}", // package@ec2
    env,
    cases,
    inputs,
    Map(),
    Map(outputFileName),
    Map(outputFileName._1 -> env),
    decorators,
    rawScript
  )

  def withNewArgs(args: Map[String, PointedTensor[Value]]): PointedPackageTensor = {
    val outScript = rawScript.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    PointedPackageTensor(name, cases, inputs, outputFileName, decorators, outScript)
  }

  def output: PointedTensor[Value.PackageOutput] = this map Value.PackageOutput

}

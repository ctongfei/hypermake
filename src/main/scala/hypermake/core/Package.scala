package hypermake.core

import cats.implicits._
import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

import scala.collection._

class Package(
    val name: Name,
    val `case`: Case,
    val inputs: Map[Name, Value],
    val outputs: (Name, Value),
    val decorators: Seq[Call],
    val rawScript: Script
)(implicit ctx: Context) {
  def on(env: Env): Task = new Task(
    name = Name(s"$name@$env"),
    env = env,
    `case` = `case`,
    inputs = inputs,
    inputEnvs = inputs.mapValuesE {
      case _: Value.Pure          => Env(Name(""))
      case _: Value.PackageOutput => env
    },
    outputFileNames = Map(outputs),
    outputEnvs = Map(outputs._1 -> env),
    decorators = decorators, // TODO: what if decorators refer to env-dependent values?
    rawScript = rawScript
  )

  def output: Value.PackageOutput = Value.PackageOutput(this)
}

/** A package can be realized on multiple environments, and cannot be dependent on any other task.
  */
case class PointedCubePackage(
    name: Name,
    cases: PointedCaseCube,
    inputs: Map[Name, PointedCube[Value]],
    outputs: (Name, PointedCube[Value]),
    decorators: Seq[PointedCubeCall],
    rawScript: PointedCube[Script]
)(implicit ctx: Context)
    extends PointedCube[Package] {

  def get(c: Case): Option[Package] = {
    if (cases containsCase c) {
      Some(
        new Package(
          name = name,
          `case` = cases.normalizeCase(c),
          inputs = inputs.mapValuesE(_.select(c).default),
          outputs = (outputs._1, outputs._2.select(c).default),
          decorators = decorators.map(_.select(c).default),
          rawScript = rawScript.select(c).default
        )
      )
    } else None
  }

  /** Returns a task that builds this package on a specific environment.
    */
  def on(env: Env)(implicit ctx: Context) = new PointedCubeTask(
    Name(s"${name.name}@${env.name}"), // package@ec2
    env,
    cases,
    inputs,
    Map(),
    Map(outputs),
    Map(outputs._1 -> env),
    decorators,
    rawScript
  )

  def withNewArgs(args: Map[Name, PointedCube[Value]]): PointedCubePackage = {
    val outScript = rawScript.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    new PointedCubePackage(name, cases, inputs, outputs, decorators, outScript)
  }

  def output: PointedCube[Value.PackageOutput] = this map Value.PackageOutput

}

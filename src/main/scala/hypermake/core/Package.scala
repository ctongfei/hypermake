package hypermake.core

import scala.collection._
import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

class Package(
               name: Name,
               `case`: Case,
               inputs: Map[Name, Value],
               decorators: Seq[Call],
               rawScript: Script
             )(implicit ctx: Context) {
  def on(env: Env): Task = new Task(
    name = Name(s"$name@$env"),
    env = env,
    `case` = `case`,
    inputs = inputs,
    inputEnvs = inputs.mapValuesE {
      case _: Value.Pure => Env(Name(""))
      case _: Value.PackageOutput => env
    },
    outputFileNames = Map(Name("package") -> Value.Pure("package")),
    outputEnvs = Map(Name("package") -> env),
    decorators = decorators, // TODO: what if decorators refer to env-dependent values?
    rawScript = rawScript
  )

  def output: Value.PackageOutput = Value.PackageOutput(this)
}

/**
 * A package can be realized on multiple environments, and cannot be dependent on any other task.
 */
case class PointedCubePackage(
                               name: Name,
                               cases: PointedCaseCube,
                               inputs: Map[Name, PointedCube[Value]],
                               decorators: Seq[PointedCubeCall],
                               rawScript: PointedCube[Script]
                             )(implicit ctx: Context)
  extends PointedCube[Package] {

  def get(c: Case): Option[Package] = {
    if (cases containsCase c) {
      Some(new Package(
        name = name,
        `case` = cases.normalizeCase(c),
        inputs = inputs.mapValuesE(_.select(c).default),
        decorators = decorators.map(_.select(c).default),
        rawScript = rawScript.select(c).default,
      ))
    } else None
  }

  /**
   * Returns a task that builds this package on a specific environment.
   */
  def on(env: Env)(implicit ctx: Context) = new PointedCubeTask(
    Name(s"${name.name}@${env.name}"), // package@ec2
    env,
    cases,
    inputs,
    Map(),
    Map(Name("package") -> PointedCube.Singleton(Value.Pure("package"))),
    Map(Name("package") -> env),
    decorators,
    rawScript
  )

  def output: PointedCube[Value.PackageOutput] = this.map(Value.PackageOutput)

}

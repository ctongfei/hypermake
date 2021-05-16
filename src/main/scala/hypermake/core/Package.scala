package hypermake.core

import scala.collection._
import hypermake.collection._
import hypermake.semantics.ParsingContext

/**
 * A package can be realized on multiple environments, and cannot be dependent on any other task.
 */
case class Package(
                   name: Name,
                   cases: PointedCaseCube,
                   inputs: Map[Name, PointedCube[Value]],
                   rawScript: PointedCube[Script]
             ) {

  /**
   * Returns a task that builds this package on a specific environment.
   */
  def on(env: Env)(implicit ctx: ParsingContext) = new PointedCubeTask(
    Name(s"${name.name}@${env.name}"),  // package@ec2
    env,
    cases,
    inputs,
    Map(),
    Map(Name("package") -> PointedCube.Singleton(Value.Pure("package"))),
    Map(Name("package") -> env),
    rawScript
  )

}

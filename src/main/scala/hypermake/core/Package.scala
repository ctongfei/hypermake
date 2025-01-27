package hypermake.core

import scala.collection._

import cats.implicits._

import hypermake.collection._
import hypermake.exception.PackageInputException
import hypermake.semantics.Context
import hypermake.util._

/**
 * Represents a package.
 * A package differs from a normal task in that it is filesystem-dependent:
 * It should be built once for each filesystem.
 */
class Package(
    val name: String,
    val `case`: Case,
    val inputs: Args[Value],
    val outputFileName: (String, Value.Pure),
    val decorators: Seq[Decorator],
    val rawScript: Script
)(implicit ctx: Context) {
  def on(fs: FileSys): Task = new Task(
    name = s"$name@$fs",
    `case` = `case`,
    inputs = Args(inputs.mapValuesE {
      case x: Value.PackageOutput => x.on(fs)
      case x                      => x
    }),
    inputFs = inputs.mapValuesE {
      case _: Value.Pure          => FileSys("")
      case _: Value.PackageOutput => fs
      case _                      => throw PackageInputException(name)
    },
    outputFileNames = Args(Map(outputFileName)),
    outputFs = Map(outputFileName._1 -> fs),
    decorators = decorators, // TODO: what if decorators refer to env-dependent values?
    rawScript = rawScript
  )

  def output: Value.PackageOutput = Value.PackageOutput(this)
}

/** A package can be realized on multiple environments, and cannot be dependent on any other task. */
case class PointedPackageTensor(
    name: String,
    shape: PointedShape,
    inputs: PointedArgsTensor[Value],
    outputFileName: (String, PointedTensor[Value.Pure]),
    decorators: Seq[PointedDecoratorTensor],
    rawScript: PointedTensor[Script]
)(implicit ctx: Context)
    extends PointedTensor[Package]
    with Partial[PointedPackageTensor] {

  def get(c: Case): Option[Package] = {
    if (shape containsCase c) {
      Some(
        new Package(
          name = name,
          `case` = shape.normalizeCase(c),
          inputs = inputs(c),
          outputFileName = outputFileName._1 -> outputFileName._2(c),
          decorators = decorators.map(_(c)),
          rawScript = rawScript(c)
        )
      )
    } else None
  }

  /** Returns a task that builds this package on a specific file system. */
  def on(fs: FileSys)(implicit ctx: Context) = new PointedTaskTensor(
    s"$name@${fs.name}", // package@ec2
    shape,
    inputs,
    Map(),
    PointedArgsTensor(Map(outputFileName)),
    Map(outputFileName._1 -> fs),
    decorators,
    rawScript
  )

  def partial(args: PointedArgsTensor[Value]): PointedPackageTensor = {
    val outScript = rawScript.productWith(args)(_ withNewArgs _)
    PointedPackageTensor(name, shape, inputs, outputFileName, decorators, outScript)
  }

  def output: PointedTensor[Value.PackageOutput] = this map Value.PackageOutput

}

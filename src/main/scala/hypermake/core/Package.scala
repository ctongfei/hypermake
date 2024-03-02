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
  def on(fs: FileSys): Task = new Task(
    name = s"$name@$fs",
    fileSys = fs,
    `case` = `case`,
    inputs = inputs,
    inputFs = inputs.mapValuesE {
      case _: Value.Pure          => FileSys("")
      case _: Value.PackageOutput => fs
    },
    outputFileNames = Map(outputFileName),
    outputFs = Map(outputFileName._1 -> fs),
    decorators = decorators, // TODO: what if decorators refer to env-dependent values?
    rawScript = rawScript
  )

  def output: Value.PackageOutput = Value.PackageOutput(this)
}

/** A package can be realized on multiple environments, and cannot be dependent on any other task.
  */
case class PointedPackageTensor(
                                 name: String,
                                 shape: PointedShape,
                                 inputs: Map[String, PointedTensor[Value]],
                                 outputFileName: (String, PointedTensor[Value.Pure]),
                                 decorators: Seq[Decorator],
                                 rawScript: PointedTensor[Script]
)(implicit ctx: Context)
    extends PointedTensor[Package] {

  def get(c: Case): Option[Package] = {
    if (shape containsCase c) {
      Some(
        new Package(
          name = name,
          `case` = shape.normalizeCase(c),
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
  def on(fs: FileSys)(implicit ctx: Context) = new PointedTaskTensor(
    s"$name@${fs.name}", // package@ec2
    fs,
    shape,
    inputs,
    Map(),
    Map(outputFileName),
    Map(outputFileName._1 -> fs),
    decorators,
    rawScript
  )

  def withNewArgs(args: Map[String, PointedTensor[Value]]): PointedPackageTensor = {
    val outScript = rawScript.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    PointedPackageTensor(name, shape, inputs, outputFileName, decorators, outScript)
  }

  def output: PointedTensor[Value.PackageOutput] = this map Value.PackageOutput

}

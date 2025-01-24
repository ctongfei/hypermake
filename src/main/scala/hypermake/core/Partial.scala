package hypermake.core

import hypermake.collection.PointedTensor

/** Represents an object that takes a set of arguments, and can be partially applied. */
trait Partial[+A] {

  /** Partial application of `this`. */
  def partial(args: PointedArgsTensor[Value]): A

  def partial(args: Args[Value]): A =
    partial(PointedArgsTensor(args.mapValuesE(v => PointedTensor.Singleton(v))))

}

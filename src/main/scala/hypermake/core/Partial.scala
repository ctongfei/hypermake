package hypermake.core

import hypermake.collection.PointedTensor

trait Partial[A] {

  def partial(args: PointedArgsTensor[Value]): A

  def partial(args: Args[Value]): A = partial(PointedArgsTensor(args.mapValuesE(v => PointedTensor.Singleton(v))))

}

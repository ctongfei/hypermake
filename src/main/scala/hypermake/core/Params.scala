package hypermake.core

import hypermake.collection._

//case class Params(params: Map[String, Option[Value]]) {
//
//  def withNewArgs(args: Map[String, Value]): Params = {
//    val newParams = params.map { case (k, v) =>
//      k -> args.get(k).orElse(v)
//    }
//    Params(newParams)
//  }
//
//}
//
//case class PointedParamsTensor(params: Map[String, PointedTensor[Value]])
//    extends PointedTensor[Params] {
//  def shape: PointedShape = params.values.foldLeft(PointedShape.singleton) { case (s, t) =>
//    s.outerJoin(t.shape)
//  }
//
//  def get(indices: Case): Option[Params] = {
//    if (shape containsCase indices) {
//      val ps = params.map { case (k, t) => k -> t.select(indices).default }
//      Some(Params(ps))
//    } else None
//  }
//}

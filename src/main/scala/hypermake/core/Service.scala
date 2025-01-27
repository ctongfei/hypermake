package hypermake.core

import hypermake.collection.{Case, PointedShape, PointedTensor}
import hypermake.exception.ObjectIsNotServiceException
import hypermake.semantics.Context
import hypermake.util.StdSinks

case class Service(rawStart: Task, stop: Task) {
  val start: Task = new Service.ServiceEphemeralTask(rawStart, this)(rawStart.ctx)
}

object Service {
  class ServiceEphemeralTask(raw: Task, service: Service)(implicit ctx: Context)
      extends Task(
        raw.name,
        raw.`case`,
        raw.inputs,
        raw.inputFs,
        raw.outputFileNames,
        raw.outputFs,
        raw.decorators,
        raw.rawScript,
        ephemeral = true
      ) {
    override def outputs: Args[Value.Output] = Args(raw.outputFileNames.keys.map { k =>
      k -> Value.Output(outputFileNames(k).value, outputFs.getOrElse(k, FileSys.local), this, Some(service))
    }.toMap)
  }
}

case class PointedServiceTensor(setup: PointedTaskTensor, teardown: PointedTaskTensor) extends PointedTensor[Service] {
  self =>

  def shape: PointedShape = setup.shape outerJoin teardown.shape

  def get(c: Case): Option[Service] = (setup productWith teardown)((s, t) => Service(s, t)).get(c)

}

object PointedServiceTensor {
  def fromObj(obj: Obj): PointedServiceTensor = {
    val start = obj.tasks.get("start").getOrElse(throw ObjectIsNotServiceException(obj))
    val stop = obj.tasks.get("stop").getOrElse(throw ObjectIsNotServiceException(obj))
    if (!(start.ephemeral && stop.ephemeral)) throw ObjectIsNotServiceException(obj)
    PointedServiceTensor(start, stop)
  }
}

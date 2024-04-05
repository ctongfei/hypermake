package hypermake.core

import hypermake.collection.{Case, PointedShape, PointedTensor}
import hypermake.exception.ObjectIsNotServiceException
import hypermake.semantics.Context

case class Service(rawSetup: Task, teardown: Task) {
  val setup: Task = new Service.ServiceEphemeralTask(rawSetup, this)(rawSetup.ctx)
}

object Service {
  class ServiceEphemeralTask(raw: Task, service: Service)(implicit ctx: Context)
      extends Task(
        raw.name,
        raw.fileSys,
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
      k -> Value.Output(outputFileNames(k).value, outputFs.getOrElse(k, fileSys), this, Some(service))
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
    val setup = obj.tasks.get("setup").getOrElse(throw ObjectIsNotServiceException(obj))
    val teardown = obj.tasks.get("teardown").getOrElse(throw ObjectIsNotServiceException(obj))
    if (!(setup.ephemeral && teardown.ephemeral)) throw ObjectIsNotServiceException(obj)
    PointedServiceTensor(setup, teardown)
  }
}

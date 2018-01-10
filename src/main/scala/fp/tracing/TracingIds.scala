package fp.tracing

import monix.eval.Task

case class TracingIds(ids: Map[String, String]) extends TracingContext {

  def asCurrent[T](t: Task[T]): Task[T] =
    TracingIds.let(this)(t)

}

object TracingIds extends TracingContextCompanion[TracingIds] {

  val default = TracingIds(Map.empty[String, String])

}

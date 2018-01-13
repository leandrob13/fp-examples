package fp.tracing

import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}

case class TracingIds(ids: Map[String, String]) extends TracingContext {
  import TracingIds._

  def asCurrent[T](t: Task[T]): Task[T] =
    TracingIds.bind(this)(t)

  def execute[T](t: Task[T])(implicit sch: Scheduler, opt: Task.Options): CancelableFuture[T] = {
    try local.bind(this)(t).runAsyncOpt
    finally local.write(default).coeval.value
  }
}

object TracingIds extends TracingContextCompanion[TracingIds] {

  val default = TracingIds(Map.empty[String, String])

}

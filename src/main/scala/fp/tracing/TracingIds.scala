package fp.tracing

import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}

case class TracingIds(ids: Map[String, String]) extends TracingContext {
  import TracingIds._

  def asCurrent[T](t: Task[T]): Task[T] =
    TracingIds.let(this)(t)

  def execute[T](t: Task[T])(implicit sch: Scheduler): CancelableFuture[T] = {
    try local.write(this)
      .flatMap(_ => t.executeWithOptions(_.enableLocalContextPropagation))
      .doOnFinish(_ => local.write(default))
      .runAsync
    finally local.write(default).coeval.value

  }
}

object TracingIds extends TracingContextCompanion[TracingIds] {

  val default = TracingIds(Map.empty[String, String])

}

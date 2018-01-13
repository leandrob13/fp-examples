package fp.tracing

import monix.eval.{Task, TaskLocal}
import monix.execution.{CancelableFuture, Scheduler}

trait TracingContext {

  def asCurrent[T](t: Task[T]): Task[T]

  def execute[T](t: Task[T])(implicit sch: Scheduler, opt: Task.Options): CancelableFuture[T]

}

trait TracingContextCompanion[T <: TracingContext] {

  val default: T

  val local: TaskLocal[T] = TaskLocal(default)

  def current: Task[T] =
    local.read.map(Option(_).getOrElse(default))

  def bind[R](t: T)(task: Task[R]): Task[R] =
    local.bind(t)(task)

}

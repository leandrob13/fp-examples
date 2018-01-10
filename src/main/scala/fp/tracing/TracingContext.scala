package fp.tracing

import monix.eval.{Task, TaskLocal}

trait TracingContext {

  def asCurrent[T](t: Task[T]): Task[T]

}

trait TracingContextCompanion[T <: TracingContext] {

  val default: T

  val local: TaskLocal[T] = TaskLocal(default)

  def current: Task[T] =
    local.read

  def let[R](t: T)(task: Task[R]) =
    local.bind(t)(task)

}

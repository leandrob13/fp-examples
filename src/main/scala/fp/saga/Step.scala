package fp.saga

import scala.collection.immutable.Queue
import monix.eval.{Coeval, Task}
import monix.execution.{CancelableFuture, Scheduler}

import scala.util.{Failure, Success, Try}



trait Step[+A, +B] extends Product with Serializable {
  import Step._

  def map[C](f: B => C): Step[A, C] = this match {
    case Next(a, b) => Next(a, f(b))
    case s @ Stop(_) => s
  }

  def flatMap[AA >: A, C](f: B => Step[AA, C]): Step[AA, C] = this match {
    case s @ Stop(_) => s
    case Next(as, b) =>
      f(b) match {
        case Next(aas, bb) => Next(as ++ aas, bb)
        case Stop(s) => Stop(as ++ s)
      }

  }

  def eval: Coeval[Either[Queue[A], B]] = this match {
    case Next(_, b) => Coeval.now(Right(b))
    case Stop(as) => Coeval.sequence(as).map(Left(_))
  }

  def task: Task[Either[Queue[A], B]] =
    Task.coeval(eval)

  def run: Try[Either[Queue[A], B]] =
    eval.runTry
}

object Step {

  final case class Next[A, B](as: Queue[Coeval[A]], b: B) extends Step[A, B]

  final case class Stop[A](as: Queue[Coeval[A]]) extends Step[A, Nothing]

  def next[A, B](a: => A, b: B): Step[A, B] =
    Next(Queue(Coeval.eval(a)), b)

  def stop[A](a: => A): Step[A, Nothing] =
    Stop(Queue(Coeval.eval(a)))

  def fromTry[A, B](t: Try[B])(revert: A): Step[A, B] = t match {
    case Success(s) => next(revert, s)
    case Failure(_) => Stop(Queue.empty[Coeval[A]])
  }
}

final case class Saga[+A, +B](t: Task[Step[A, B]]) {

  def map[C](f: B => C) =
    Saga {
      t.map {
        case Step.Next(as, b) => Step.Next(as, f(b))
        case s @ Step.Stop(_) => s
      }
    }

  def flatMap[AA >: A, C](f: B => Saga[AA, C]): Saga[AA, C] =
    Saga {
      t.flatMap {
        case Step.Next(as, b) => f(b).t flatMap  {
          case Step.Next(aas, bb) => Task(Step.Next(as enqueue aas, bb))
          case Step.Stop(s) => Task(Step.Stop(as enqueue s))
        }
        case s @ Step.Stop(_) => Task(s)
      }
    }

  def coeval(implicit s : Scheduler): Coeval[Either[CancelableFuture[Step[A, B]], Step[A, B]]] =
    t.coeval

  def eval: Task[Either[Queue[A], B]] =
    t.flatMap(_.task)

  def run(implicit s : Scheduler): CancelableFuture[Either[Queue[A], B]] =
    eval.runAsync
}



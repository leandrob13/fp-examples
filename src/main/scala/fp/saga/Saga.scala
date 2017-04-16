package fp.saga

import scala.collection.immutable.Queue
import monix.execution.Scheduler.Implicits.global
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture

import scala.util.{Failure, Success, Try}



trait Saga[+A, +B] extends Product with Serializable {
  import Saga._

  def map[C](f: B => C): Saga[A, C] = this match {
    case Next(a, b) => Next(a, f(b))
    case s @ Stop(_) => s
  }

  def flatMap[AA >: A, C](f: B => Saga[AA, C]): Saga[AA, C] = this match {
    case s @ Stop(_) => s
    case Next(as, b) =>
      f(b) match {
        case Next(aas, bb) => Next(as ++ aas, bb)
        case Stop(s) => Stop(as ++ s)
      }

  }

  def run: Either[Queue[Try[A]], B] = this match {
    case Next(_, b) => Right(b)
    case Stop(as) => Left(as.map(_.runTry))
  }
}

case class SagaT[A, B](t: Task[Saga[A, B]]) {

  def flatMap[AA >: A, C](f: B => SagaT[AA, C]): SagaT[AA, C] = {
    SagaT {
      t.flatMap {
        case Saga.Next(as, b) => f(b).t flatMap  {
          case Saga.Next(aas, bb) => Task(Saga.Next(as ++ aas, bb))
          case Saga.Stop(s) => Task(Saga.Stop(as ++ s))
        }
        case s @ Saga.Stop(_) => Task(s)
      }
    }
  }
  def run: CancelableFuture[Either[Queue[Try[A]], B]] = t.runAsync.map(_.run)
}

object Saga {

  case class Next[A, B](as: Queue[Coeval[A]], b: B) extends Saga[A, B]
  case class Stop[A](as: Queue[Coeval[A]]) extends Saga[A, Nothing]

  def next[A, B](a: => A, b: B): Saga[A, B] = Next(Queue(Coeval(a)), b)
  def stop[A](a: => A): Saga[A, Nothing] = Stop(Queue(Coeval(a)))

  def fromTry[A, B](t: Try[B])(revert: A): Saga[A, B] = t match {
    case Success(s) => next(revert, s)
    case Failure(_) => Stop(Queue.empty[Coeval[A]])
  }
}

package fp.saga

import fp.saga.Step.{Next, Stop}
import monix.eval.Coeval
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

class StepTest extends WordSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global

  var start = 1

  case class Sum(a: Int)
  case class Rest(a: Int)

  def sumStart(b: Int): Step[Int, Int] = {
    Step.next({start -= b; start}, {start += b; start})
  }

  //last step: there is no mutation of start but it depends on its state.
  //To give an example of a stateless operation in a saga.
  def trySum(a: String) = Try(a.toInt + start) match {
    case Success(r) => Next(Queue.empty[Coeval[Unit]], r)
    case Failure(_) => Stop(Queue.empty[Coeval[Unit]])
  }

  "Saga" should {

    "Revert" in {
      val res = for {
        a <- sumStart(2)
        b <- sumStart(2)
        z <- trySum("s")
      } yield z
      //state changes how it should, no recovery until run
      start shouldBe 5
      val done: Try[Either[Queue[AnyVal], Int]] = res.run
      println(s"DONE $done")
      //revert is applied and state should be the original
      start shouldBe 1
    }

    "End successfuly" in {
      val res = for {
        a <- sumStart(2)
        b <- sumStart(2)
        z <- trySum("2")
      } yield z
      start shouldBe 5 //to state lazyness
      val done: Try[Either[Queue[AnyVal], Int]] = res.run
      done shouldBe Success(Right(7))
      start shouldBe 5
    }
  }

}

package fp.tracing

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Inside, MustMatchers, WordSpec}

import scala.concurrent.Future


class TracingIdsTest extends WordSpec with MustMatchers with ScalaFutures with Inside {

  val tracing = TracingIds.default.copy(ids = Map("transaction" -> "1234"))

  "TracingIds" should {
    "propagate task" in {
      implicit val scheduler: Scheduler = Scheduler.global

      implicit val opts = Task.defaultOptions.enableLocalContextPropagation
      println(s"A ${Thread.currentThread().getName}")

      //val t = tracing.asCurrent(Task.fork(TracingIdsTest.checkTask("transaction")))
      val t = tracing.asCurrent(TracingIdsTest.checkTask("transaction"))

      println(s"======Before ${TracingIds.current.coeval.value}")
      whenReady(tracing.execute(TracingIdsTest.checkTask("transaction"))) { res =>
        println(s"A ${Thread.currentThread().getName}")
        res must matchPattern { case Some("1234") => }
      }
      println(s"======Aafter ${TracingIds.current.coeval.value}")
    }

    /*"not propagate task" in {
      implicit val scheduler: Scheduler = Scheduler.global

      implicit val opts = Task.defaultOptions.disableLocalContextPropagation

      val fut = TracingIdsTest.checkTask("transaction")//.runAsyncOpt
      whenReady(fut.runAsync) { res =>
        res must matchPattern { case None => }
      }
    }*/

    /*"propagate future" in {
      implicit val scheduler: Scheduler = Scheduler.global
      implicit val opts = Task.defaultOptions.enableLocalContextPropagation

      val fut = tracing.asCurrent(Task.deferFuture(TracingIdsTest.checkFuture("transaction")))
        .runAsyncOpt

      whenReady(fut) { res =>
        res must matchPattern { case Some("1234") => }
      }
    }*/
  }

}

object TracingIdsTest {

  def checkTask(id: String): Task[Option[String]] = {
    TracingIds.current.asyncBoundary.map { t =>
      println(s"B ${Thread.currentThread().getName}")
      println(s"ids $id ${t.ids.get(id)}")
      t.ids.get(id)
    }
  }

  def checkFuture(id: String)(implicit sch: Scheduler): Future[Option[String]] = {
    TracingIds.current.runAsync.map { t =>
      println(s"ids $id ${t.ids.get(id)}")
      t.ids.get(id)
    }
  }
}
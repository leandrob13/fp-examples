package fp.tracing

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Inside, MustMatchers, WordSpec}

import scala.concurrent.Future


class TracingIdsTest extends WordSpec with MustMatchers with ScalaFutures with Inside {

  implicit val scheduler: Scheduler = Scheduler.global

  implicit val opts: Task.Options = Task.defaultOptions.enableLocalContextPropagation

  val tracing = TracingIds.default.copy(ids = Map("transaction" -> "1234"))

  "TracingIds" should {

    "asCurrent propagates task without async boundary" in {

      val t = tracing.asCurrent(TracingIdsTest.checkTask("transaction"))

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)

      whenReady(t.runAsyncOpt) { res =>
        res must matchPattern { case Some("1234") => }
      }

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)
    }

    "asCurrent propagates task with async boundary" in {

      val t = tracing.asCurrent(TracingIdsTest.checkTask("transaction").executeWithFork)

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)

      whenReady(t.runAsyncOpt) { res =>
        res must matchPattern { case Some("1234") => }
      }

      TracingIds.current.coeval.value mustBe Right(tracing)
    }

    "execute propagates task without async boundary" in {

      val t = tracing.execute(TracingIdsTest.checkTask("transaction"))

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)

      whenReady(t) { res =>
        res must matchPattern { case Some("1234") => }
      }

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)
    }

    "execute propagates task with async boundary" in {

      val t = tracing.execute(TracingIdsTest.checkTask("transaction").executeWithFork)

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)

      whenReady(t) { res =>
        res must matchPattern { case Some("1234") => }
      }

      TracingIds.current.coeval.value mustBe Right(TracingIds.default)
    }

    "not propagate task" in {

      val t = TracingIdsTest.checkTask("transaction")
      whenReady(t.runAsyncOpt) { res =>
        res must matchPattern { case None => }
      }
    }

    "propagate future" in {

      val fut = tracing.asCurrent(Task.deferFuture(TracingIdsTest.checkFuture("transaction")))
        .runAsyncOpt

      whenReady(fut) { res =>
        res must matchPattern { case Some("1234") => }
      }
    }
  }

}

object TracingIdsTest {

  def checkTask(id: String): Task[Option[String]] =
    TracingIds.current.map(_.ids.get(id))

  def checkFuture(id: String)(implicit sch: Scheduler): Future[Option[String]] =
    TracingIds.current.runAsync.map(_.ids.get(id))
}
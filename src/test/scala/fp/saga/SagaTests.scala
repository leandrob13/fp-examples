package fp.saga

import monix.eval.{Coeval, Task}
import org.scalatest.{Matchers, WordSpec}
import scala.concurrent.duration._
import scala.collection.immutable.Queue
import scala.collection.mutable.{Map => mMap}
import scala.concurrent.Await
import scala.util.Try

class SagaTests extends WordSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global

  case class Person(id: Int, name: String)

  val db: mMap[Int, String] = mMap(1 ->"henry", 2 -> "thomas")

  def upsert(n: Int, name: String): Option[String] =
    db.put(n, name)

  def delete(n: Int): Option[String] = {
    db.get(n) match {
      case Some(_) =>
        db.remove(n)
      case None =>
        //We make it throw an exception to recover with Task
        throw new Exception("no value")
    }
  }

  def upsertPerson(p: Person): Saga[Option[String], Option[String]] = Saga {
    Task {
      upsert(p.id, p.name) match {
        case Some(v) =>
          //In case the key had a previous value, the revert should be
          //to return to previous state for that key
          Step.next(upsert(p.id, v), upsert(p.id, p.name))
        case None =>
          //If there was no previous value for that key, the revert
          //operation should be to delete
          Step.next(delete(p.id), upsert(p.id, p.name))
      }
    }
  }

  def deletePerson(n: Int): Saga[Unit, Option[String]] = Saga {
    Task(Step.Next(Queue.empty[Coeval[Unit]], delete(n))).onErrorRecoverWith {
      case ex => Task.now(Step.Stop(Queue.empty[Coeval[Unit]]))
    }
  }


  "SagaT" should {
    "Revert" in {
      val res: Saga[Any, Unit] = for {
        a <- upsertPerson(Person(3, "mathew"))
        b <- deletePerson(10)
        c <- upsertPerson(Person(2, "john"))
      } yield ()
      val done = res.run
      val awaited: Either[Queue[Any], Unit] = Await.result(done, 5.seconds)
      println(s"MAP fail $db")
      db shouldBe mMap(1 ->"henry", 2 -> "thomas")
    }

    "End successfully" in {
      val res: Saga[Any, Unit] = for {
        a <- upsertPerson(Person(3, "mathew"))
        b <- deletePerson(1)
        c <- upsertPerson(Person(2, "john"))
      } yield ()
      val done = res.run
      val awaited: Either[Queue[Any], Unit] = Await.result(done, 5.seconds)
      println(s"MAP success $db")
      db shouldBe mMap(2 -> "john", 3 -> "mathew")
    }
  }
}

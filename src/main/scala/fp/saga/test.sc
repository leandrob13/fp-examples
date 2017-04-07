import cats.data.EitherT
import scala.concurrent.Future
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global

val x = EitherT(Future.failed[Either[String, Int]](new Exception("failed")))
val y = EitherT(Future.successful[Either[String, Int]](Right(1)))

val z = for {
  a <- y
  b <- x
} yield (a, b)
z.isLeft
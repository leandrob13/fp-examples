package fp.scalaTry

import cats.data.NonEmptyList
import org.scalatest.{Inside, Matchers, WordSpec}

import scala.util.{Failure, Success}


class BookValidationTrySpec extends WordSpec with Matchers with Inside with BookValidationService {
  import fp.BookData._

  "BookValidation" should {

    "Validate a book" in {
      val validated = validateBook(theFountainhead)
      validated should === (Success(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate a book applicative" in {
      val validated = validateBookAp(theFountainhead)
      validated should === (Success(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate books" in {
      val validatedBooks = validateBooks(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Success(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Validate books applicative" in {
      val validatedBooks = validateBooksAp(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Success(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Fail fast error on a book" in {
      val validated = validateBook(titlelessBook)
      inside(validated) {
        case Failure(ex: InvalidParameter) => ex.getMessage shouldBe "title must not be empty"
      }
    }

    "Fail fast error a book applicative" in {
      val validated = validateBookAp(titlelessBook)
      inside(validated) {
        case Failure(ex) => ex.getMessage shouldBe "title must not be empty"
      }
    }

    "Fail fast error on books" in {
      val validatedBooks = validateBooks(List(titlelessBook, theCountOfMontecristo, theFountainhead))
      inside(validatedBooks) {
        case Failure(ex) => ex.getMessage shouldBe "title must not be empty"
      }
    }

    "Fail fast error on books applicative" in {
      val validatedBooks = validateBooksAp(List(theCountOfMontecristo, titlelessBook, atlasShrugged))
      inside(validatedBooks) {
        case Failure(ex) => ex.getMessage shouldBe "isbn has not a valid format"
      }
    }
  }
}

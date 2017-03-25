package fp.either

import cats.data.NonEmptyList
import fp.InvalidParameter
import org.scalatest.{Matchers, WordSpec}

class BookValidationEitherSpec extends WordSpec with Matchers with BookValidationService {
  import fp.BookData._

  "BookValidation" should {

    "Validate a book" in {
      val validated = validateBook(theFountainhead)
      validated should === (Right(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate a book applicative" in {
      val validated = validateBookAp(theFountainhead)
      validated should === (Right(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate books" in {
      val validatedBooks = validateBooks(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Right(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Validate books applicative" in {
      val validatedBooks = validateBooksAp(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Right(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Fail fast error on a book" in {
      val validated = validateBook(titlelessBook)
      validated should === (Left(InvalidParameter("title must not be empty")))
    }

    "Fail fast error a book applicative" in {
      val validated = validateBookAp(titlelessBook)
      validated should === (Left(InvalidParameter("title must not be empty")))
    }

    "Fail fast error on books" in {
      val validatedBooks = validateBooks(List(titlelessBook, theCountOfMontecristo, theFountainhead))
      validatedBooks should === (Left(InvalidParameter("title must not be empty")))
    }

    "Fail fast error on books applicative" in {
      val validatedBooks = validateBooksAp(List(theCountOfMontecristo, titlelessBook, atlasShrugged))
      validatedBooks should === (Left(InvalidParameter("isbn has not a valid format")))
    }
  }
}

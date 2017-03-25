package fp.ior

import cats.data.{Ior, NonEmptyList}
import fp.{Book, Genre, InvalidParameter}
import org.scalatest.{Matchers, WordSpec}


class BookValidationIorSpec extends WordSpec with Matchers with BookValidationService {
  import fp.BookData._

  "BookValidation" should {

    "Validate a book" in {
      val validated = validateBook(theFountainhead)
      validated should === (Ior.Right(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate a book applicative" in {
      val validated = validateBookAp(theFountainhead)
      validated should === (Ior.Right(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate books" in {
      val validatedBooks = validateBooks(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Ior.Right(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Validate books applicative" in {
      val validatedBooks = validateBooksAp(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Ior.Right(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Fail fast error on a book" in {
      val validated = validateBook(titlelessBook)
      validated should === (Ior.Left(NonEmptyList(InvalidParameter("title must not be empty"), Nil)))
    }

    "Fail fast error a book applicative" in {
      val validated = validateBookAp(titlelessBook)
      validated should === (Ior.Left(NonEmptyList(InvalidParameter("title must not be empty"), Nil)))
    }

    "Fail fast error on books" in {
      val validatedBooks = validateBooks(List(titlelessBook, theCountOfMontecristo, theFountainhead, atlasShrugged))
      validatedBooks should === (
        Ior.Both(
          NonEmptyList(
            InvalidParameter("title must not be empty"),
            InvalidParameter("isbn has not a valid format") :: Nil
          ),
          NonEmptyList(
            theFountainhead, atlasShrugged :: Nil
          )
        )
      )
    }

    "Fail fast error on books applicative" in {
      val validatedBooks = validateBooksAp(List(theCountOfMontecristo, titlelessBook, theFountainhead, atlasShrugged))
      validatedBooks should === (
        Ior.Both(
          NonEmptyList(
            InvalidParameter("isbn has not a valid format"),
            InvalidParameter("title must not be empty") :: Nil
          ),
          NonEmptyList(
            theFountainhead, atlasShrugged :: Nil
          )
        )
      )
    }
  }
}

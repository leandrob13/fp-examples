package fp.validated

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import fp.InvalidParameter
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by leandrob13 on 3/25/17.
  */
class BookValidationValidatedSpec extends WordSpec with Matchers with BookValidationService {
  import fp.BookData._

  "BookValidation" should {

    "Validate a book" in {
      val validated = validateBook(theFountainhead)
      validated should === (Valid(NonEmptyList(theFountainhead, Nil)))
    }

    "Validate books" in {
      val validatedBooks = validateBooks(List(theFountainhead, atlasShrugged))
      validatedBooks should === (Valid(NonEmptyList(theFountainhead, atlasShrugged :: Nil)))
    }

    "Accumulate errors on a book" in {
      val validated = validateBook(titlelessBook)
      validated should === (Invalid(NonEmptyList(InvalidParameter("title must not be empty"), InvalidParameter("Book has invalid genre") :: Nil)))
    }

    "Accumulate errors on books" in {
      val validatedBooks = validateBooks(List(titlelessBook, theCountOfMontecristo, theFountainhead))
      validatedBooks should === (Invalid(NonEmptyList(InvalidParameter("title must not be empty"), InvalidParameter("Book has invalid genre") :: InvalidParameter("isbn has not a valid format") :: Nil)))
    }


  }
}

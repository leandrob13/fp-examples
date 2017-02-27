package fp.validated

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.cartesian._
import cats.syntax.semigroup._
import cats.syntax.validated._
import fp.Genre.InvalidGenre
import fp.{Book, EmptyBookList, Genre, InvalidParameter}

import scala.util.matching.Regex

trait BookValidationService {

  private val isbnRegex: Regex =
    """ISBN(?:-13)?:?\x20*(?=.{17}$)97(?:8|9)([ -])\d{1,5}\1\d{1,7}\1\d{1,6}\1\d$""".r

  def validateBooks(bs: List[Book]) = bs match {
    case Nil => EmptyBookList("Book list was empty").invalidNel[Book]
    case books => books map validateBook reduce (_ |+| _)
  }

  def validateBook(b: Book): ValidatedNel[InvalidParameter, NonEmptyList[Book]] = ( validateIsbn(b.isbn) |@|
    validateAuthor(b.author) |@|
    validateTitle(b.title) |@|
    validateGenre(b.genre) ) map {
    case (isbn, author, title, genre) =>
      NonEmptyList.of(Book(isbn, title, author, genre))
  }

  private def validateGenre(g: Genre): ValidatedNel[InvalidParameter, Genre] = g match {
    case InvalidGenre => InvalidParameter("Book has invalid genre").invalidNel
    case genre => genre.validNel
  }

  private def validateIsbn(isbn: String): ValidatedNel[InvalidParameter, String] = isbn match {
    case isbnRegex(all @ _*) => isbn.validNel
    case _ => InvalidParameter("isbn has not a valid format").invalidNel
  }

  private def validateTitle(title: String): ValidatedNel[InvalidParameter, String] =
    if (title.isEmpty) InvalidParameter("title must not be empty").invalidNel else title.validNel

  private def validateAuthor(author: String): ValidatedNel[InvalidParameter, String] =
    if (author.isEmpty) InvalidParameter("author must not be empty").invalidNel else author.validNel

}


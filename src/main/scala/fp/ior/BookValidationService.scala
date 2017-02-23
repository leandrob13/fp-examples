package fp.ior

import cats.data.{Ior, ValidatedNel}
import cats.syntax.cartesian._
import cats.syntax.semigroup._
import cats.syntax.validated._
import fp.ior.Genre.InvalidGenre

import scala.util.matching.Regex

trait BookValidationService {

  def validateBooks(bs: List[Book]): IorNel[Error, Book] = bs match {
    case Nil => EmptyBookList("Book list was empty").toLeftIorNel
    case books => books map validateBook reduce (_ |+| _)
  }

  def validateBook(b: Book): IorNel[InvalidBookParameter, Book] = {
    val validations = ( validateIsbn(b.isbn) |@|
      validateTitle(b.author) |@|
      validateTitle(b.title) |@|
      validateGenre(b.genre) ) map {
      case (isbn, author, title, genre) =>
        Book(isbn, title, author, genre)
    }
    validations.fold(Ior.left, _.toRightIorNel)
  }

  private def validateGenre(g: Genre): ValidatedNel[InvalidBookParameter, Genre] = g match {
    case InvalidGenre => InvalidBookParameter("Book has invalid genre").invalidNel
    case genre => genre.validNel
  }

  private def validateIsbn(isbn: String): ValidatedNel[InvalidBookParameter, String] =
    if (isbnRegex.findFirstIn(isbn).isEmpty) InvalidBookParameter("isbn has not a valid format").invalidNel
    else isbn.validNel

  private def validateTitle(title: String): ValidatedNel[InvalidBookParameter, String] =
    if (title.isEmpty) InvalidBookParameter("title must not be empty").invalidNel else title.validNel

  private def validateAuthor(author: String): ValidatedNel[InvalidBookParameter, String] =
    if (author.isEmpty) InvalidBookParameter("author must not be empty").invalidNel else author.validNel

  lazy val isbnRegex: Regex =
    """ISBN(?:-13)?:?\x20*(?=.{17}$)97(?:8|9)([ -])\d{1,5}\1\d{1,7}\1\d{1,6}\1\d$""".r

}

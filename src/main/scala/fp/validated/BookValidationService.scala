package fp.validated

import cats.data.ValidatedNel
import cats.syntax.cartesian._
import cats.syntax.validated._
import fp.Genre.InvalidGenre
import fp.{Book, Genre, InvalidBookParameter}

import scala.util.matching.Regex

trait BookValidationService {

  private val isbnRegex: Regex =
    """ISBN(?:-13)?:?\x20*(?=.{17}$)97(?:8|9)([ -])\d{1,5}\1\d{1,7}\1\d{1,6}\1\d$""".r


  def validateBook(b: Book) = ( validateIsbn(b.isbn) |@|
    validateAuthor(b.author) |@|
    validateTitle(b.title) |@|
    validateGenre(b.genre) ) map {
    case (isbn, author, title, genre) =>
      Book(isbn, title, author, genre)
  }

  private def validateGenre(g: Genre): ValidatedNel[InvalidBookParameter, Genre] = g match {
    case InvalidGenre => InvalidBookParameter("Book has invalid genre").invalidNel
    case genre => genre.validNel
  }

  private def validateIsbn(isbn: String): ValidatedNel[InvalidBookParameter, String] = isbn match {
    case isbnRegex(all @ _*) => isbn.validNel
    case _ => InvalidBookParameter("isbn has not a valid format").invalidNel
  }

  private def validateTitle(title: String): ValidatedNel[InvalidBookParameter, String] =
    if (title.isEmpty) InvalidBookParameter("title must not be empty").invalidNel else title.validNel

  private def validateAuthor(author: String): ValidatedNel[InvalidBookParameter, String] =
    if (author.isEmpty) InvalidBookParameter("author must not be empty").invalidNel else author.validNel

}


package fp.either

import cats.data.NonEmptyList
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.cartesian._
import cats.syntax.semigroup._
import fp.Genre.InvalidGenre
import fp.{Book, EmptyBookList, Error, Genre, InvalidParameter}

import scala.util.matching.Regex

trait BookValidationService {

  private val isbnRegex: Regex =
    """ISBN(?:-13)?:?\x20*(?=.{17}$)97(?:8|9)([ -])\d{1,5}\1\d{1,7}\1\d{1,6}\1\d$""".r

  def validateBooks(bs: List[Book]): Either[Error, NonEmptyList[Book]] = bs match {
    case Nil => EmptyBookList("Book list was empty").asLeft
    case books => books map validateBook reduce (_ |+| _)
  }

  def validateBooksAp(bs: List[Book]): Either[Error, NonEmptyList[Book]] = bs match {
    case Nil => EmptyBookList("Book list was empty").asLeft
    case books => books map validateBookAp reduce (_ |+| _)
  }

  def validateBook(b: Book): Either[InvalidParameter, NonEmptyList[Book]] = {
    for {
      i <- validateIsbn(b.isbn)
      a <- validateAuthor(b.author)
      t <- validateTitle(b.title)
      g <- validateGenre(b.genre)
    } yield NonEmptyList.of(Book(i, t, a, g))
  }

  def validateBookAp(b: Book): Either[InvalidParameter, NonEmptyList[Book]] = (
    validateIsbn(b.isbn) |@|
      validateAuthor(b.author) |@|
      validateTitle(b.title) |@|
      validateGenre(b.genre) ) map {
    case (isbn, author, title, genre) =>
      NonEmptyList.of(Book(isbn, title, author, genre))
  }

  private def validateGenre(g: Genre): Either[InvalidParameter, Genre] = g match {
    case InvalidGenre => InvalidParameter("Book has invalid genre").asLeft
    case genre => genre.asRight
  }

  private def validateIsbn(isbn: String): Either[InvalidParameter, String] = isbn match {
    case isbnRegex(all @ _*) => isbn.asRight
    case _ => InvalidParameter("isbn has not a valid format").asLeft
  }

  private def validateTitle(title: String): Either[InvalidParameter, String] =
    if (title.isEmpty) InvalidParameter("title must not be empty").asLeft else title.asRight

  private def validateAuthor(author: String): Either[InvalidParameter, String] =
    if (author.isEmpty) InvalidParameter("author must not be empty").asLeft else author.asRight
}

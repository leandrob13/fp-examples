package fp.scalaTry

import cats.data.NonEmptyList
import fp.{Book, Genre}
import cats.instances.try_._
import cats.syntax.semigroup._
import cats.syntax.cartesian._
import scala.util.{Failure, Try}
import scala.util.matching.Regex


trait BookValidationService {

  private val isbnRegex: Regex =
    """ISBN(?:-13)?:?\x20*(?=.{17}$)97(?:8|9)([ -])\d{1,5}\1\d{1,7}\1\d{1,6}\1\d$""".r

  def validateBooks(bs: List[Book]): Try[NonEmptyList[Book]] = bs match {
    case Nil => Failure(new EmptyBookList("Book list was empty"))
    case books => books map validateBook reduce (_ |+| _)
  }

  def validateBooksAp(bs: List[Book]): Try[NonEmptyList[Book]] = bs match {
    case Nil => throw new EmptyBookList("Book list was empty")
    case books => books map validateBookAp reduce (_ |+| _)
  }

  def validateBook(b: Book): Try[NonEmptyList[Book]] =
    for {
      i <- validateIsbn(b.isbn)
      a <- validateAuthor(b.author)
      t <- validateTitle(b.title)
      g <- validateGenre(b.genre)
    } yield NonEmptyList.of(Book(i, t, a, g))

  def validateBookAp(b: Book): Try[NonEmptyList[Book]] = (
    validateIsbn(b.isbn) |@|
      validateAuthor(b.author) |@|
      validateTitle(b.title) |@|
      validateGenre(b.genre) ) map {
    case (isbn, author, title, genre) =>
      NonEmptyList.of(Book(isbn, title, author, genre))
  }

  private def validateGenre(g: Genre): Try[Genre] = Try {
    g match {
      case Genre.InvalidGenre => throw new InvalidParameter("Book has invalid genre")
      case genre => genre
    }
  }

  private def validateIsbn(isbn: String): Try[String] = Try {
    isbn match {
      case isbnRegex(all @ _*) => isbn
      case _ => throw new InvalidParameter("isbn has not a valid format")
    }
  }

  private def validateTitle(title: String): Try[String] = Try {
    if (Option(title).forall(_.isEmpty)) throw new InvalidParameter("title must not be empty") else title
  }

  private def validateAuthor(author: String): Try[String] = Try {
    if (Option(author).forall(_.isEmpty)) throw new InvalidParameter("author must not be empty") else author
  }
}

class InvalidParameter(message: String) extends Exception(message)
class EmptyBookList(message: String) extends Exception(message)

package fp.ior

import cats.data.{Ior, NonEmptyList}
import cats.syntax.semigroup._
import fp.Genre.InvalidGenre
import fp.{Book, EmptyBookList, Error, Genre, InvalidBookParameter}

import scala.util.matching.Regex

trait BookValidationService {

  private val isbnRegex: Regex =
    """ISBN(?:-13)?:?\x20*(?=.{17}$)97(?:8|9)([ -])\d{1,5}\1\d{1,7}\1\d{1,6}\1\d$""".r

  def validateBooks(bs: List[Book]): IorNel[Error, Book] = bs match {
    case Nil => EmptyBookList("Book list was empty").toLeftIorNel
    case books => books map validateBook reduce (_ |+| _)
  }

  def validateBook(b: Book): IorNel[InvalidBookParameter, Book] = {
    val validations: Ior[InvalidBookParameter, NonEmptyList[Book]] = for {
      i <-  validateIsbn(b.isbn)
      a <-  validateAuthor(b.author)
      t <-  validateTitle(b.title)
      g <-  validateGenre(b.genre)
    } yield NonEmptyList.of(Book(i, t, a, g))
    validations.leftMap(NonEmptyList.of(_))
  }
  private def validateGenre(g: Genre): Ior[InvalidBookParameter, Genre] = g match {
    case InvalidGenre => Ior.left(InvalidBookParameter("Book has invalid genre"))
    case genre => Ior.right(genre)
  }

  private def validateIsbn(isbn: String): Ior[InvalidBookParameter, String] = isbn match {
    case isbnRegex(all @ _*) => Ior.right(isbn)
    case _ => Ior.left(InvalidBookParameter("isbn has not a valid format"))
  }

  private def validateTitle(title: String): Ior[InvalidBookParameter, String] =
    if (title.isEmpty) Ior.left(InvalidBookParameter("title must not be empty"))
    else Ior.right(title)

  private def validateAuthor(author: String): Ior[InvalidBookParameter, String] =
    if (author.isEmpty) Ior.left(InvalidBookParameter("author must not be empty"))
    else Ior.right(author)





}

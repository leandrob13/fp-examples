package fp

case class Book(isbn: String, title: String, author: String, genre: Genre)

trait Genre extends Product with Serializable

object Genre {

  def apply(g: String): Genre = g match {
    case "science-fiction" => ScienceFiction
    case "historic-novel" => HistoricNovel
    case _ => InvalidGenre
  }

  case object ScienceFiction extends Genre
  case object HistoricNovel extends Genre
  case object InvalidGenre extends Genre
}

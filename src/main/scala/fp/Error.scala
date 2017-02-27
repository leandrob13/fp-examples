package fp

sealed trait Error extends Product with Serializable {
  val message: String
}

case class InvalidParameter(message: String) extends Error
case class EmptyBookList(message: String) extends Error

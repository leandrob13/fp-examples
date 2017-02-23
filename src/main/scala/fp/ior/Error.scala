package fp.ior


sealed trait Error extends Product with Serializable {
  val message: String
}

case class InvalidBookParameter(message: String) extends Error
case class EmptyBookList(message: String) extends Error

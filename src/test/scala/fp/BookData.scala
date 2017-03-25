package fp


object BookData {

  val theFountainhead = Book(
    isbn = "ISBN: 978-1-4028-9462-6",
    title = "The Fountainhead",
    author = "Ayn Rand",
    genre = Genre.Fiction
  )

  val atlasShrugged = Book(
    isbn = "ISBN-13 978-1-4028-9462-6",
    title = "Atlas Shrugged",
    author = "Ayn Rand",
    genre = Genre.Fiction
  )

  val theCountOfMontecristo = Book(
    isbn = "ISBN-13 978-1-4028-9462",
    title = "The Count Of Montecristo",
    author = "Alexandre Dumas",
    genre = Genre.HistoricNovel
  )

  val titlelessBook = Book(
    isbn = "ISBN-13 978-1-4028-9462-6",
    title = "",
    author = "Unknown",
    genre = Genre.InvalidGenre
  )
}

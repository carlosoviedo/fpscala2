case class Book(title: String, authors: Set[String])

val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = Set("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = Set("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = Set("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = Set("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = Set("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

val authorsWithBirdName =
  for (b <- books; a <- b.authors if a startsWith "Bird, ")
    yield b.title

val booksWithProgramInTitle =
  for (b <- books if b.title.indexOf("Program") >= 0)
    yield b.title

val authorsWith2Books =
  for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

val authorsWithBirdNameHOF =
  books flatMap(b => b.authors.withFilter(a => a startsWith "Bird, ").map(a => b.title))

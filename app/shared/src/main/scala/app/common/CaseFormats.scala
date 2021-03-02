package app.common

import hydro.common.GuavaReplacement.Splitter

object CaseFormats {

  def tokenize(s: String): Seq[String] = {
    // Convert to ASCII alphanumeric characters + underscores
    val normalized = {
      s.replaceAll("[^a-zA-Z0-9]+", "_")
    }

    var result = Seq(normalized)

    // Separate in words by separator
    result = result.flatMap(r => Splitter.on('_').omitEmptyStrings().split(r))

    // Separate camelCase words
    result = result.flatMap(parseCamelCase)

    // Convert all strings to lowercase
    result = result.map(_.toLowerCase)

    result
  }

  def toUpperCamelCase(tokens: Seq[String]): String = {
    tokens.map(s => s.head.toUpper + s.tail).mkString
  }

  def toSnakeCase(tokens: Seq[String]): String = {
    tokens.mkString("_")
  }

  def toDashCase(tokens: Seq[String]): String = {
    tokens.mkString("-")
  }

  private def parseCamelCase(s: String): Seq[String] = {
    // If there is no case difference, return the input
    if (s == s.toLowerCase) Seq(s)
    else if (s == s.toUpperCase) Seq(s)

    // There is a case difference --> parse into words
    else {
      def inner(lastWord: String, remainder: List[Char]): List[String] = {
        remainder match {
          case Nil                          => lastWord :: Nil
          case head :: tail if head.isUpper => lastWord :: inner(lastWord = head.toString, remainder = tail)
          case head :: tail if head.isLower => inner(lastWord = lastWord + head, remainder = tail)
        }
      }

      inner(lastWord = "", remainder = s.toList).filter(_.nonEmpty)
    }
  }
}

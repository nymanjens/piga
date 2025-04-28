package app.common

import scala.collection.immutable.Seq
import hydro.common.GuavaReplacement.Splitter

import scala.collection.immutable.ListMap
import scala.collection.mutable

object MarkdownConverter {

  private val wordBreakCharacters: Set[Char] = " \n.,".toSet

  def markdownToParsedTasks(s: String): Seq[ParsedTask] = {
    val markdown = s.trim
    if (s.startsWith("- ")) {
      for (task <- splitInTasks(s)) yield {
        ParsedTask(
          html = toHtml(task.trim.stripPrefix("- ")),
          relativeIndentation = task.prefixLength(_ == ' ') / 2,
        )
      }
    } else {
      Seq(ParsedTask(html = toHtml(markdown), relativeIndentation = 0))
    }
  }

  private def splitInTasks(s: String): Seq[String] = {
    val result = mutable.Buffer[String]()
    var currentLine = ""
    for (line <- Splitter.on('\n').omitEmptyStrings().split(s)) {
      if (currentLine.isEmpty) {
        currentLine = line
      } else if (line.trim.startsWith("- ")) {
        result.append(currentLine)
        currentLine = line
      } else {
        currentLine += "\n" + line.trim
      }
    }
    result.append(currentLine)
    result.toVector
  }

  // Not using general-purpose library because Piga doesn't support all HTML tags
  private def toHtml(markdown: String): String = {
    def innerImplementation(markdown: String, supportLinks: Boolean): String = {
      val markdownToHtmlSymbols: Map[String, String] = ListMap(
        "**" -> "b",
        "*" -> "i",
        "_" -> "i",
        "`" -> "code",
        "~" -> "s",
      )

      val result = StringBuilder.newBuilder

      var index = 0
      while (index < markdown.length) {
        val char = markdown.charAt(index)

        val matchingStartSymbol = markdownToHtmlSymbols.keys.find(markdown.substring(index).startsWith)
        val atWordBreak = index == 0 || wordBreakCharacters.contains(markdown.charAt(index - 1))

        def handleMarkdownSymbol(): Boolean = {
          matchingStartSymbol match {
            case Some(symbol) if atWordBreak =>
              val indexAfterSymbol = index + symbol.length
              maybeClosingSymbolIndex(markdown.substring(indexAfterSymbol), symbol) match {
                case Some(indexDelta) =>
                  val innerMarkdown = markdown.substring(indexAfterSymbol, indexAfterSymbol + indexDelta)
                  val innerHtml = innerImplementation(innerMarkdown, supportLinks = supportLinks)
                  val htmlSymbol = markdownToHtmlSymbols(symbol)

                  result.append(f"<$htmlSymbol>$innerHtml</$htmlSymbol>")
                  index = indexAfterSymbol + indexDelta + symbol.length
                  true
                case None =>
                  false
              }
            case _ => false
          }
        }

        def handleLink(): Boolean = {
          val linkRegex = """^\[(.*?)\]\((.*?)\)""".r.unanchored
          markdown.substring(index) match {
            case linkRegex(name, url) =>
              val fullMatch = f"[$name]($url)"
              require(markdown.substring(index).startsWith(fullMatch))
              val processedName = innerImplementation(name, supportLinks = false)

              result.append(f"""<a href="$url">$processedName</a>""")
              index += fullMatch.length
              true
            case _ => false
          }
        }

        def handleGeneralCase(): Boolean = {
          result.append(char match {
            case '<' => "&lt;"
            case '>' => "&gt;"
            case _   => char.toString
          })
          index += 1
          true
        }

        require(handleMarkdownSymbol() || (supportLinks && handleLink()) || handleGeneralCase())
      }

      result.toString
    }
    innerImplementation(markdown, supportLinks = true)
  }

  private def maybeClosingSymbolIndex(string: String, symbol: String): Option[Int] = {
    for (index <- string.indices) {
      if (string.substring(index).startsWith(symbol)) {
        val stringAfterSymbol = string.substring(index + symbol.length)
        if (stringAfterSymbol.isEmpty || wordBreakCharacters.contains(stringAfterSymbol.head)) {
          return Some(index)
        }
      }
    }
    None
  }

  case class ParsedTask(html: String, relativeIndentation: Int)
}

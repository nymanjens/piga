package app.common

import scala.collection.immutable.Seq
import hydro.common.GuavaReplacement.Splitter

import scala.collection.mutable

object MarkdownConverter {

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

  private def toHtml(markdown: String): String = {
    var result = markdown
    result = symbolToHtml(result, markdownSymbol = "**", htmlSymbol = "b")
    result = symbolToHtml(result, markdownSymbol = "*", htmlSymbol = "i")
    result = symbolToHtml(result, markdownSymbol = "_", htmlSymbol = "i")
    result = symbolToHtml(result, markdownSymbol = "`", htmlSymbol = "code")
    result = symbolToHtml(result, markdownSymbol = "~", htmlSymbol = "s")
    result = urlToHtml(result)
    result
  }

  private def symbolToHtml(markdown: String, markdownSymbol: String, htmlSymbol: String): String = {
    val parts = split(markdown, separator = markdownSymbol)
    var insideSymbols = false
    var lastInsideSymbolPart = ""
    val resultBuilder = new StringBuilder(parts.head)
    for (part <- parts.drop(1)) {
      insideSymbols = !insideSymbols
      if (insideSymbols) {
        lastInsideSymbolPart = part
      } else {
        resultBuilder.append(f"<$htmlSymbol>$lastInsideSymbolPart</$htmlSymbol>$part")
      }
    }
    if (insideSymbols) {
      resultBuilder.append(markdownSymbol + lastInsideSymbolPart)
    }
    resultBuilder.toString()
  }

  private def urlToHtml(markdown: String): String = {
    markdown.replaceAll("""\[(.*?)\]\((.*?)\)""", """<a href="$2">$1</a>""")
  }

  private def split(s: String, separator: String): Seq[String] = {
    val result = mutable.Buffer[String]()
    var currentPart = new StringBuilder()
    var i = 0
    while (i < s.length)
      if (s.substring(i).startsWith(separator)) {
        result.append(currentPart.toString())
        currentPart = new StringBuilder()
        i += separator.length
      } else {
        currentPart.append(s(i))
        i += 1
      }
    result.append(currentPart.toString())
    result.toVector
  }

  case class ParsedTask(html: String, relativeIndentation: Int)
}

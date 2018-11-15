package flux.stores.document

import common.ScalaUtils.visibleForTesting
import models.document.Document
import models.document.Document.{IndexedCursor, IndexedSelection}
import org.scalajs.dom

import scala.util.matching.Regex

final class DocumentSelectionStore {

  def setSelection(document: Document, indexedSelection: IndexedSelection): Unit = {
    dom.window.localStorage.setItem(localStorageKey(document), serialize(indexedSelection))
  }

  def getSelection(document: Document): IndexedSelection = {
    dom.window.localStorage.getItem(localStorageKey(document)) match {
      case null => defaultSelection
      case item => deserialize(item)
    }
  }

  @visibleForTesting private[document] def serialize(selection: IndexedSelection): String = {
    val IndexedSelection(start, end) = selection
    s"${start.seqIndex},${start.offsetInTask};${end.seqIndex},${end.offsetInTask}"
  }
  private val deserializeRegex: Regex = raw"(\d+),(\d+);(\d+),(\d+)".r
  @visibleForTesting private[document] def deserialize(string: String): IndexedSelection = string match {
    case deserializeRegex(startIndex, startOffset, endIndex, endOffset) =>
      IndexedSelection(
        IndexedCursor(startIndex.toInt, startOffset.toInt),
        IndexedCursor(endIndex.toInt, endOffset.toInt))
    case _ => defaultSelection
  }

  private def localStorageKey(document: Document): String = {
    s"doc-sel-${document.id}"
  }

  private def defaultSelection: IndexedSelection = IndexedSelection.singleton(IndexedCursor(0, 0))
}

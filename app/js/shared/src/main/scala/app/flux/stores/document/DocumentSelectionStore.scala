package app.flux.stores.document

import hydro.common.Annotations.visibleForTesting
import app.models.document.Document
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import org.scalajs.dom

import scala.util.matching.Regex

final class DocumentSelectionStore {

  def setSelection(documentId: Long, indexedSelection: IndexedSelection): Unit = {
    dom.window.localStorage.setItem(localStorageKey(documentId), serialize(indexedSelection))
  }

  def getSelection(documentId: Long): IndexedSelection = {
    dom.window.localStorage.getItem(localStorageKey(documentId)) match {
      case null => IndexedSelection.nullInstance
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
        IndexedCursor(endIndex.toInt, endOffset.toInt),
      )
    case _ => IndexedSelection.nullInstance
  }

  private def localStorageKey(documentId: Long): String = {
    s"doc-sel-${documentId}"
  }
}

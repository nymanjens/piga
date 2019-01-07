package app.flux.stores.document

import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import utest._

object DocumentSelectionStoreTest extends TestSuite {

  override def tests = TestSuite {

    val store: DocumentSelectionStore = new DocumentSelectionStore()

    "serialize and deserialize" - {
      def testBackAndForth(selection: IndexedSelection) = {
        store.deserialize(store.serialize(selection)) ==> selection
      }

      testBackAndForth(IndexedSelection.singleton(IndexedCursor(0, 0)))
      testBackAndForth(IndexedSelection.singleton(IndexedCursor(10, 11)))
      testBackAndForth(IndexedSelection(IndexedCursor(10, 11), IndexedCursor(100, 121)))
    }
  }
}

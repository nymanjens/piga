package flux.stores.document

import api.ScalaJsApi.UserPrototype
import common.testing.Awaiter
import common.testing.TestObjects._
import flux.action.Action
import models.document.Document.{IndexedCursor, IndexedSelection}
import models.modification.EntityModification
import scala2js.Converters._
import utest._

import scala.async.Async.{async, await}
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

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

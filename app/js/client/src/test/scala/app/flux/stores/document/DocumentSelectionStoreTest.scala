package app.flux.stores.document

import app.api.ScalaJsApi.UserPrototype
import hydro.common.testing.Awaiter
import common.testing.TestObjects._
import app.flux.action.AppActions
import hydro.flux.action.StandardActions
import app.models.document.Document.IndexedCursor
import app.models.document.Document.IndexedSelection
import app.models.modification.EntityModification
import hydro.scala2js.StandardConverters._
import app.scala2js.AppConverters._
import utest._

import scala.async.Async.async
import scala.async.Async.await
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

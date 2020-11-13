package app.common.testing

import app.api.ScalaJsApi.GetInitialDataResponse
import app.flux.react.app.document.EditHistory
import app.flux.stores.document.AllDocumentsStore
import app.flux.stores.document.DocumentSelectionStore
import app.flux.stores.document.DocumentStoreFactory
import hydro.common.testing.FakeClock
import hydro.common.testing.FakeI18n
import hydro.common.testing.FakeJsEntityAccess
import hydro.common.testing.FakeRouterContext
import hydro.flux.action.Dispatcher
import hydro.models.access.HydroPushSocketClientFactory

import scala.collection.immutable.Seq

class TestModule {

  // ******************* Fake implementations ******************* //
  implicit lazy val fakeEntityAccess = new FakeJsEntityAccess
  implicit lazy val fakeClock = new FakeClock
  implicit lazy val fakeDispatcher = new Dispatcher.Fake
  implicit lazy val fakeI18n = new FakeI18n
  implicit lazy val testUser = TestObjects.testUser
  implicit lazy val fakeScalaJsApiClient = new FakeScalaJsApiClient
  implicit lazy val fakeRouterContext = new FakeRouterContext
  implicit val fakeGetInitialDataResponse = GetInitialDataResponse(
    user = testUser,
    allAccessibleDocuments = Seq(),
    i18nMessages = Map(),
    nextUpdateToken = "",
  )

  // ******************* Non-fake implementations ******************* //
  implicit lazy val hydroPushSocketClientFactory: HydroPushSocketClientFactory =
    new HydroPushSocketClientFactory
  implicit lazy val documentSelectionStore: DocumentSelectionStore = new DocumentSelectionStore
  implicit lazy val documentStoreFactory: DocumentStoreFactory = new DocumentStoreFactory
  implicit lazy val editHistory: EditHistory = new EditHistory
  implicit lazy val allDocumentsStore: AllDocumentsStore = new AllDocumentsStore
}

package common.testing

import flux.stores.document.DocumentSelectionStore
import hydro.flux.action.Dispatcher
import models.access.EntityModificationPushClientFactory

class TestModule {

  // ******************* Fake implementations ******************* //
  implicit lazy val fakeEntityAccess = new FakeJsEntityAccess
  implicit lazy val fakeClock = new FakeClock
  implicit lazy val fakeDispatcher = new Dispatcher.Fake
  implicit lazy val fakeI18n = new FakeI18n
  implicit lazy val testUser = TestObjects.testUser
  implicit lazy val fakeScalaJsApiClient = new FakeScalaJsApiClient
  implicit lazy val fakeRouterContext = new FakeRouterContext

  // ******************* Non-fake implementations ******************* //
  implicit lazy val entityModificationPushClientFactory: EntityModificationPushClientFactory =
    new EntityModificationPushClientFactory
  implicit lazy val documentSelectionStore: DocumentSelectionStore = new DocumentSelectionStore
}

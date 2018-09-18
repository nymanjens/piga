package flux.stores

import java.time.Instant

import common.LoggingUtils.logExceptions
import common.time.Clock
import common.time.JavaTimeImplicits._
import common.{I18n, Unique}
import flux.action.Action._
import flux.action.{Action, Dispatcher}
import flux.stores.GlobalMessagesStore.Message
import models.access.EntityAccess

import scala.concurrent.duration._
import scala.scalajs.js

final class GlobalMessagesStore(implicit i18n: I18n,
                                clock: Clock,
                                entityAccess: EntityAccess,
                                dispatcher: Dispatcher)
    extends StateStore[Option[Message]] {
  dispatcher.registerPartialSync(dispatcherListener)

  private var _state: Option[Unique[Message]] = None

  // **************** Public API ****************//
  override def state: Option[Message] = _state.map(_.get)

  // **************** Private dispatcher methods ****************//
  private def dispatcherListener: PartialFunction[Action, Unit] = {
    case action if getCompletionMessage.isDefinedAt(action) =>
      setState(Message(string = i18n("app.sending-data-to-server"), messageType = Message.Type.Working))

    case Action.Done(action) =>
      getCompletionMessage.lift.apply(action) match {
        case Some(message) =>
          setState(Message(string = message, messageType = Message.Type.Success))
          clearMessageAfterDelay()
        case None =>
      }

    case Action.Failed(action) =>
      getCompletionMessage.lift.apply(action) match {
        case Some(message) =>
          setState(
            Message(string = i18n("app.sending-data-to-server-failed"), messageType = Message.Type.Failure))
          clearMessageAfterDelay()
        case None =>
      }

    case Action.SetPageLoadingState( /* isLoading = */ false) =>
      if (state.isDefined && state.get.age > java.time.Duration.ofSeconds(3)) {
        setState(None)
      }
  }

  private def getCompletionMessage: PartialFunction[Action, String] = {
    // **************** Document-related actions **************** //
    case _: AddEmptyDocument =>
      i18n("app.successfully-added-document")
    case UpdateDocuments(documents) =>
      if (documents.size == 1) i18n("app.successfully-updated-document")
      else i18n("app.successfully-updated-documents")
    case RemoveDocument(document) =>
      i18n("app.successfully-removed-document")
    // **************** User-related actions **************** //
    case UpsertUser(userPrototype)
        if userPrototype.id.isDefined && userPrototype.plainTextPassword.isDefined =>
      i18n("app.successfully-updated-password")
    case UpsertUser(userPrototype) if userPrototype.id.isEmpty =>
      i18n("app.successfully-added-user", userPrototype.loginName getOrElse "<Unknown name>")
  }

  /** Clear this message after some delay */
  private def clearMessageAfterDelay(): Unit = {
    // Note: The delay is large because we don't want everything on the page to suddenly move up one row
    // while it is being used. This is expected to trigger when a user has left the page open while doing
    // something else.
    val uniqueStateWhenCreatedMessage = _state
    js.timers.setTimeout(2.minutes)(logExceptions {
      if (_state == uniqueStateWhenCreatedMessage) {
        // state has remained unchanged since start of timer
        setState(None)
      }
    })
  }

  // **************** Private state helper methods ****************//
  private def setState(message: Message): Unit = {
    setState(Some(message))
  }
  private def setState(state: Option[Message]): Unit = {
    _state = state.map(Unique.apply)
    invokeStateUpdateListeners()
  }
}

object GlobalMessagesStore {
  case class Message private (string: String, messageType: Message.Type, private val createTime: Instant) {
    private[GlobalMessagesStore] def age(implicit clock: Clock): java.time.Duration =
      java.time.Duration.between(createTime, clock.nowInstant)
  }

  object Message {
    def apply(string: String, messageType: Message.Type)(implicit clock: Clock): Message =
      Message(string = string, messageType = messageType, createTime = clock.nowInstant)

    sealed trait Type
    object Type {
      object Working extends Type
      object Success extends Type
      object Failure extends Type
    }
  }
}

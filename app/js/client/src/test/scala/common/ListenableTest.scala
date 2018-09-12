package common

import common.Listenable.WritableListenable
import common.testing.Awaiter
import utest.{TestSuite, _}

import scala.async.Async.{async, await}
import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala2js.Converters._

import scala.util.Success

object ListenableTest extends TestSuite {

  override def tests = TestSuite {

    "mergeWith()" - {
      val l1 = WritableListenable(123)
      val l2 = WritableListenable(456)
      val sum = Listenable.mergeWith[Int](_ + _)(l1, l2)

      "get" - async {
        sum.get ==> 579

        l2.set(-1)
        await(Awaiter.expectEventually.equal(sum.get, 122))
      }
      "calls listener when value changes" - async {
        val promise = Promise[Int]()
        sum.registerListener(newValue => promise.success(newValue))

        l2.set(-1)

        await(Awaiter.expectEventually.complete(promise.future, 122))
      }
      "only registers listener if listened to" - {
        val listener1: Listenable.Listener[Int] = _ => {}
        val listener2: Listenable.Listener[Int] = _ => {}

        l1.hasListeners ==> false
        sum.registerListener(listener1)
        l1.hasListeners ==> true
        sum.registerListener(listener2)
        l1.hasListeners ==> true
        sum.deregisterListener(listener1)
        l1.hasListeners ==> true
        sum.deregisterListener(listener2)
        l1.hasListeners ==> false
      }
    }

    "fromFuture()" - {
      "from unresolved future" - {
        val listenableBackingPromise = Promise[Int]()
        val listenable: Listenable[Option[Int]] = Listenable.fromFuture(listenableBackingPromise.future)

        "get" - async {
          listenable.get ==> None

          listenableBackingPromise.success(123)

          await(Awaiter.expectEventually.equal(listenable.get, Some(123)))
        }
        "calls listener when value changes" - async {
          val promise = Promise[Option[Int]]()
          listenable.registerListener(newValue => promise.success(newValue))

          listenableBackingPromise.success(777)

          await(Awaiter.expectEventually.complete(promise.future, Some(777)))
        }
      }
      "from completed future" - {
        val listenable: Listenable[Option[Int]] = Listenable.fromFuture(Future.successful(123))

        "get" - async {
          listenable.get ==> Some(123)
        }
      }
    }

    "WritableListenable" - {
      val listenable: WritableListenable[Int] = WritableListenable(123)

      "get" - {
        listenable.get ==> 123
      }
      "set" - {
        listenable.set(456)
        listenable.get ==> 456
      }
      "calls listener when value changes" - async {
        val promise = Promise[Int]()
        listenable.registerListener(newValue => promise.success(newValue))

        listenable.set(456)

        await(Awaiter.expectEventually.complete(promise.future, 456))
      }
      "doesn't call listener when value stays the same" - async {
        val promise = Promise[Int]()
        listenable.registerListener(newValue => promise.success(newValue))

        listenable.set(123)

        await(Awaiter.expectConsistently.neverComplete(promise.future))
      }
    }

    "ListenableMap" - {
      // TODO
    }

    "map" - {
      // TODO
    }

    "flatMap" - {
      // TODO
      // TODO: Test that listeners are deregistered
    }
  }
}

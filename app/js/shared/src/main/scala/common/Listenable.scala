package common

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import common.Listenable.{Listener, MappedListenable, WritableListenable}

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Success

sealed trait Listenable[T] {
  def get: T
  def registerListener(listener: Listener[T]): Unit
  def deregisterListener(listener: Listener[T]): Unit

  final def map[U](mappingFunction: T => U): Listenable[U] = {
    flatMap(mappingFunction = t => Listenable.fixed(mappingFunction(t)))
  }
  final def flatMap[U](mappingFunction: T => Listenable[U]): Listenable[U] = {
    new MappedListenable(this, mappingFunction)
  }
}

object Listenable {

  def fixed[T](value: T): Listenable[T] = new Listenable[T] {
    override def get: T = value
    override def registerListener(listener: Listener[T]): Unit = {}
    override def deregisterListener(listener: Listener[T]): Unit = {}
  }

  def mergeWith[T](func: (T, T) => T)(l1: Listenable[T], l2: Listenable[T]): Listenable[T] = ???

  def fromFuture[T](future: Future[T]): Listenable[Option[T]] = future.value match {
    case Some(Success(value)) => fixed(Some(value))
    case _ =>
      val result = WritableListenable[Option[T]](None)
      future.map(value => result.set(Some(value)))
      result
  }

  trait Listener[T] {
    def onChange(newValue: T): Unit
  }

  final class WritableListenable[T](initialValue: T) extends Listenable[T] {
    private var value: T = initialValue
    private[Listenable] var listeners: Seq[Listener[T]] = Seq()

    override def get: T = value
    override def registerListener(listener: Listener[T]): Unit = {
      listeners = listeners :+ listener
    }
    override def deregisterListener(listener: Listener[T]): Unit = {
      listeners = listeners.filter(_ != listener)
    }

    def set(newValue: T): Unit = {
      val oldValue = value
      if (oldValue != newValue) {
        value = newValue
        listeners.foreach(_.onChange(newValue))
      }
    }
  }
  object WritableListenable {
    def apply[T](value: T): WritableListenable[T] = new WritableListenable[T](value)
  }

  final class ListenableMap[K, V] extends Listenable[Map[K, V]] {
    private var delegateMap: mutable.Map[K, V] = mutable.Map()
    private var listeners: Seq[Listener[Map[K, V]]] = Seq()

    override def get: Map[K, V] = delegateMap.toMap
    override def registerListener(listener: Listener[Map[K, V]]): Unit = {
      listeners = listeners :+ listener
    }
    override def deregisterListener(listener: Listener[Map[K, V]]): Unit = {
      listeners = listeners.filter(_ != listener)
    }

    def contains(key: K): Boolean = delegateMap contains key
    def put(key: K, value: V): Unit = {
      val previousValue: Option[V] = delegateMap.put(key, value)
      if (previousValue != Some(value)) {
        val newValue = get
        listeners.foreach(_.onChange(newValue))
      }
    }
    def apply(key: K): V = delegateMap(key)
  }
  object ListenableMap {
    def apply[K, V](): ListenableMap[K, V] = new ListenableMap[K, V]
  }

  private final class MappedListenable[T, U](origin: Listenable[T], mappingFunction: T => U)
      extends Listenable[U] {
    private val delegate: WritableListenable[U] = WritableListenable(mappingFunction(origin.get))

    override def get: U = delegate.get
    override def registerListener(listener: Listener[U]): Unit = {
      if (delegate.listeners.isEmpty) {
        origin.registerListener(OriginListener)
      }
      delegate.registerListener(listener)
    }
    override def deregisterListener(listener: Listener[U]): Unit = {
      delegate.deregisterListener(listener)
      if (delegate.listeners.isEmpty) {
        origin.deregisterListener(OriginListener)
      }
    }

    private object OriginListener extends Listener[T] {
      override def onChange(newOriginValue: T): Unit = {
        val newValue = mappingFunction(newOriginValue)
        delegate.set(newValue)
      }
    }
  }
}

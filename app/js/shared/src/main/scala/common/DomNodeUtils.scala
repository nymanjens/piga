package common

import japgolly.scalajs.react.CallbackTo
import org.scalajs.dom
import org.scalajs.dom.console

import scala.concurrent.{ExecutionContext, Future}

object DomNodeUtils {

  def asTextNode(node: dom.raw.Node): Option[dom.raw.Text] = {
    if (node.nodeType == dom.raw.Node.TEXT_NODE) {
      Some(node.asInstanceOf[dom.raw.Text])
    } else {
      None
    }
  }

  def nodeIsLi(node: dom.raw.Node): Boolean = nodeIsElement(node, "LI")
  def nodeIsBr(node: dom.raw.Node): Boolean = nodeIsElement(node, "BR")
  def nodeIsDiv(node: dom.raw.Node): Boolean = nodeIsElement(node, "DIV")

  def nodeIsElement(node: dom.raw.Node, tagName: String): Boolean = {
    node.nodeType == dom.raw.Node.ELEMENT_NODE &&
    node.asInstanceOf[dom.raw.Element].tagName == tagName
  }

  def walkDepthFirstPreOrder(node: dom.raw.Node): Iterable[NodeWithOffset] = {
    var offsetSoFar = 0
    def internal(node: dom.raw.Node): Iterable[NodeWithOffset] = {
      val nodeLength = asTextNode(node).map(_.length) getOrElse 0
      val nodeWithOffset = NodeWithOffset(node, offsetSoFar, offsetAtEnd = offsetSoFar + nodeLength)
      offsetSoFar += nodeLength

      val iterables = for (i <- 0 until node.childNodes.length) yield {
        internal(node.childNodes.item(i))
      }
      nodeWithOffset +: iterables.flatten
    }
    internal(node)
  }

  case class NodeWithOffset(node: dom.raw.Node, offsetSoFar: Int, offsetAtEnd: Int)
}

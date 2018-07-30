package common

import japgolly.scalajs.react.CallbackTo
import org.scalajs.dom
import org.scalajs.dom.console

import scala.concurrent.{ExecutionContext, Future}

object DomNodeUtils {

  def nodeIsLi(node: dom.raw.Node): Boolean = nodeIsElement(node, "LI")
  def nodeIsBr(node: dom.raw.Node): Boolean = nodeIsElement(node, "BR")
  def nodeIsDiv(node: dom.raw.Node): Boolean = nodeIsElement(node, "DIV")

  def nodeIsElement(node: dom.raw.Node, tagName: String): Boolean = {
    node.nodeType == dom.raw.Node.ELEMENT_NODE &&
    node.asInstanceOf[dom.raw.Element].tagName == tagName
  }
}

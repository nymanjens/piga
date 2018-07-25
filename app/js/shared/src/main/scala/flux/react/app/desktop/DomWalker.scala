package flux.react.app.desktop

import org.scalajs.dom

private[desktop] object DomWalker {

  def depthFirstPreOrder(node: dom.raw.Node): Iterable[NodeWithOffset] = {
    var offsetSoFar = 0
    def internal(node: dom.raw.Node): Iterable[NodeWithOffset] = {
      val nodeLength =
        if (node.nodeType == dom.raw.Node.TEXT_NODE) node.asInstanceOf[dom.raw.Text].length else 0
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

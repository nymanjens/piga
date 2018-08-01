package common

import common.ScalaUtils.visibleForTesting

import scala.annotation.tailrec

case class OrderToken @visibleForTesting private[common] (private val parts: List[Int])
    extends Ordered[OrderToken] {
  require(parts.nonEmpty)
  require(
    parts.tail.isEmpty || parts.last != OrderToken.missingPartValue,
    s"Redundant ${OrderToken.missingPartValue} at end of $parts")

  override def compare(that: OrderToken): Int = {
    @tailrec
    def innerCompare(parts1: List[Int], parts2: List[Int]): Int = (parts1, parts2) match {
      case (Nil, Nil)                                         => 0
      case (Nil, _)                                           => innerCompare(OrderToken.missingPartValue :: Nil, parts2)
      case (_, Nil)                                           => innerCompare(parts1, OrderToken.missingPartValue :: Nil)
      case (head1 :: rest1, head2 :: rest2) if head1 == head2 => innerCompare(rest1, rest2)
      case (head1 :: _, head2 :: _)                           => head1 compare head2
    }
    innerCompare(this.parts, that.parts)
  }
}

object OrderToken {
  private val missingPartValue = 0

  def middleBetween(lower: Option[OrderToken], higher: Option[OrderToken]): OrderToken = {
    require(
      lower.isEmpty || higher.isEmpty || lower.get <= higher.get,
      s"Not true that lower=$lower <= higher=$higher")

    def splitHeadAndNext(listOption: Option[List[Int]], noneOptionValue: Int): (Long, Option[List[Int]]) =
      listOption match {
        case None               => (noneOptionValue.toLong, None)
        case Some(Nil)          => (missingPartValue.toLong, Some(Nil))
        case Some(head :: rest) => (head.toLong, Some(rest))
      }

    type HasCarryOver = Boolean
    def middleBetweenWithCarryOver(lower: Option[List[Int]],
                                   higher: Option[List[Int]]): (List[Int], HasCarryOver) = {
      val (lowerHead, lowerNext) = splitHeadAndNext(lower, Int.MinValue)
      val (higherHead, higherNext) = splitHeadAndNext(higher, Int.MaxValue)

      if (lowerHead == Int.MaxValue && higherHead == Int.MinValue) {
        val (resultRest, resultHasCarryOver) = middleBetweenWithCarryOver(lowerNext, higherNext)
        if (resultHasCarryOver) {
          (higherHead.toInt :: resultRest, resultHasCarryOver)
        } else {
          (lowerHead.toInt :: resultRest, resultHasCarryOver)
        }
      } else {
        val result = (lowerHead + (higherHead + Int.MaxValue.toLong)) / 2
        if (result <= Int.MaxValue) {
          (result.toInt :: Nil, /* HasCarryOver */ false)
        } else {
          ((result - Int.MaxValue).toInt :: Nil, /* HasCarryOver */ true)
        }
      }
    }

    def innerMiddleBetween(lower: Option[List[Int]], higher: Option[List[Int]]): List[Int] = {
      val (lowerHead, lowerNext) = splitHeadAndNext(lower, Int.MinValue)
      val (higherHead, higherNext) = splitHeadAndNext(higher, Int.MaxValue)

      if (lowerHead == higherHead) {
        lowerHead.toInt :: innerMiddleBetween(lowerNext, higherNext)
      } else if (higherHead - lowerHead >= 2) {
        ((lowerHead + higherHead) / 2).toInt :: Nil
      } else if (higherHead - lowerHead == 1) {
        val (resultRest, resultHasCarryOver) = middleBetweenWithCarryOver(lowerNext, higherNext)
        if (resultHasCarryOver) {
          higherHead.toInt :: resultRest
        } else {
          lowerHead.toInt :: resultRest
        }
      } else {
        throw new IllegalArgumentException(s"lowerHead=$lowerHead, higherHead=$higherHead")
      }
    }

    if (lower.isDefined && lower == higher) {
      lower.get
    } else {
      OrderToken(innerMiddleBetween(lower.map(_.parts), higher.map(_.parts)))
    }
  }
}

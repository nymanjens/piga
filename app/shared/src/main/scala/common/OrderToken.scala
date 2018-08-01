package common

import common.ScalaUtils.visibleForTesting

import scala.annotation.tailrec

case class OrderToken private (@visibleForTesting private[common] val parts: List[Int])
    extends Ordered[OrderToken] {

  override def compare(that: OrderToken): Int = {
    @tailrec
    def innerCompare(parts1: List[Int], parts2: List[Int]): Int = (parts1, parts2) match {
      case (Nil, Nil)                                         => 0
      case (Nil, _)                                           => innerCompare(OrderToken.missingPartValue :: Nil, parts2)
      case (_, Nil)                                           => innerCompare(parts1, OrderToken.missingPartValue :: Nil)
      case (head1 :: rest1, head2 :: rest2) if head1 == head2 => innerCompare(rest1, rest2)
      case (head1 :: _, head2 :: _)                           => head1 - head2
    }
    innerCompare(this.parts, that.parts)
  }
}

object OrderToken {
  private val missingPartValue = 0

  def middleBetween(lower: Option[OrderToken], higher: Option[OrderToken]): OrderToken = {
    require(lower <= higher)

    def splitHeadAndNext(listOption: Option[List[Int]], noneOptionValue: Int): (Int, Option[List[Int]]) =
      listOption match {
        case None               => (noneOptionValue, None)
        case Some(Nil)          => (missingPartValue, Some(Nil))
        case Some(head :: rest) => (head, Some(rest))
      }

    type HasCarryOver = Boolean
    def middleBetweenWithCarryOver(lower: Option[List[Int]],
                                   higher: Option[List[Int]]): (List[Int], HasCarryOver) = {
      val (lowerHead, lowerNext) = splitHeadAndNext(lower, Int.MinValue)
      val (higherHead, higherNext) = splitHeadAndNext(higher, Int.MaxValue)

      if (lowerHead == Int.MaxValue && higherHead == Int.MinValue) {
        val (resultRest, resultHasCarryOver) = middleBetweenWithCarryOver(lowerNext, higherNext)
        if (resultHasCarryOver) {
          (higherHead :: resultRest, resultHasCarryOver)
        } else {
          (lowerHead :: resultRest, resultHasCarryOver)
        }
      } else {
        val result = (lowerHead.toLong + (higherHead.toLong + Int.MaxValue.toLong)) / 2
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
        lowerHead :: innerMiddleBetween(lowerNext, higherNext)
      } else if (higherHead - lowerHead >= 2) {
        ((lowerHead.toLong + higherHead.toLong) / 2).toInt :: Nil
      } else if (higherHead - lowerHead == 1) {
        val (resultRest, resultHasCarryOver) = middleBetweenWithCarryOver(lowerNext, higherNext)
        if (resultHasCarryOver) {
          higherHead :: resultRest
        } else {
          lowerHead :: resultRest
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

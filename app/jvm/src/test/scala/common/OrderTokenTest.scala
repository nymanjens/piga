package common

import org.specs2.execute.ResultLike
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

class OrderTokenTest extends Specification {

  "equality" in {
    OrderToken(List(1, 2)) mustEqual OrderToken(List(1, 2))
    OrderToken(List(1, 2)) mustNotEqual OrderToken(List(1, 2, 1))
  }

  "compareTo" in {
    def testCompare(lesser: OrderToken, greater: OrderToken): MatchResult[OrderToken] = {
      lesser must beLessThan(greater)
      greater must beGreaterThan(lesser)
    }
    testCompare(lesser = OrderToken(List(1)), greater = OrderToken(List(2)))
    testCompare(lesser = OrderToken(List(1, 2, -100)), greater = OrderToken(List(1, 2)))
    testCompare(lesser = OrderToken(List(1, 2)), greater = OrderToken(List(1, 2, 100)))
    testCompare(lesser = OrderToken(List(1, 2, Int.MinValue)), greater = OrderToken(List(1, 2, Int.MaxValue)))
  }

  "middleBetween" in {
    "manual test cases" in {
      OrderToken.middleBetween(None, None) mustEqual OrderToken(List(0))
    }
    "always append" in {
      def tokenStream(previousValue: OrderToken): Stream[OrderToken] = {
        val thisValue = OrderToken.middleBetween(Some(previousValue), None)
        thisValue #:: tokenStream(previousValue = thisValue)
      }
      tokenStream(OrderToken.middleBetween(None, None)).take(100).toSeq mustEqual 1
    }

    // TODO: Test that no trailing 0 is added at the end
  }
}

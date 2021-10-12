

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Lottery.segmentFinder
import Lottery.Segment

class SegmentFinderSpec extends AnyFlatSpec with Matchers {

  "A segmentFinder" should "count segments correctly" in {
  segmentFinder.countContainingSegments(Array(1, 6, 11), Array(Segment(0, 5), Segment(7, 10))) shouldEqual Array(1, 0, 0)
  }


}



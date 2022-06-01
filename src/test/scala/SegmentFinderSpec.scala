import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SegmentFinderSpec extends AnyFlatSpec with Matchers {

  "A segmentFinder" should "count segments correctly" in {
  segmentFinder.countContainingSegments(Array(1, 6, 11), Array(Segment(0, 5), Segment(7, 10))) shouldEqual Array(1, 0, 0)
  segmentFinder.countContainingSegments(Array(-100, 100, 0), Array(Segment(-10, 10))) shouldEqual Array(0, 0, 1)
  segmentFinder.countContainingSegments(Array(1, 6), Array(Segment(0, 5), Segment(-3, 2), Segment(7, 10))) shouldEqual Array(2, 0)
  }


}

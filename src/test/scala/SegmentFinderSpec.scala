import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}

class SegmentFinderSpec extends AnyFlatSpec with Matchers {

  "A segmentFinder" should "count segments correctly" in {

    val examples = Table(
      ("points", "segments", "expRes"),
      (Array(1, 6, 11), Array(Segment(0, 5), Segment(7, 10)), Array(1, 0, 0)),
      (Array(-100, 100, 0), Array(Segment(-10, 10)), Array(0, 0, 1)),
      (Array(1, 6), Array(Segment(0, 5), Segment(-3, 2), Segment(7, 10)), Array(2, 0)),
      (Array(0), Array(Segment(1, 3)), Array(0)),
      (Array(3), Array(Segment(1, 3)), Array(1)),
      (Array(3, 3), Array(Segment(1, 3)), Array(1, 1)),
      (Array(5, 5, 5), Array(Segment(4, 5), Segment(4, 5)), Array(2, 2, 2)),
      (Array(2, 3, 5, 5), Array(Segment(1, 5), Segment(2, 5), Segment(3, 5), Segment(4, 5)), Array(2, 3, 4, 4)),
    )

    forAll(examples){
      (points: Array[Int], segments: Array[Segment], expRes: Array[Int]) => {
        val res = segmentFinder.countContainingSegments(points, segments)
        res shouldEqual expRes
      }
    }

  }


}

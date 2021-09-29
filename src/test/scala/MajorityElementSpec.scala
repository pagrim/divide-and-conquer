import org.scalatest.flatspec.AnyFlatSpec
import MajorityElement.majorityElement

class MajorityElementSpec extends AnyFlatSpec {

  "An Majority Element" should "identify majority value" in {
    assert(majorityElement.getMajority(Array(2, 3, 9, 2, 2)) == 2)
    assert(majorityElement.getMajority(Array(1, 2, 3, 4)) == 0)
    assert(majorityElement.getMajority(Array(1, 2, 3, 1)) == 0)
  }

}


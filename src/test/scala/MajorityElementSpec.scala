import org.scalatest.flatspec.AnyFlatSpec


class MajorityElementSpec extends AnyFlatSpec {

  "An Majority Element" should "identify majority value" in {
    assert(majorityElement.getMajority(Array(1, 2, 5, 3, 5, 4, 5, 5, 5, 6)) == (-1, 0))
    assert(majorityElement.getMajority(Array(2, 3, 9, 2, 2)) == (2, 1))
    assert(majorityElement.getMajority(Array(1, 2, 3, 4)) == (-1, 0))
    assert(majorityElement.getMajority(Array(1, 2, 3, 1)) == (-1, 0))
  }

}

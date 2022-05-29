import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}


class QuickSortSpec extends AnyFlatSpec with Matchers {

  "A quickSort" should "sort input array correctly" in {
    quickSort.sort(Array(4, 1, 5, 8, 9, 5), randomSeed = Some(42)) shouldEqual Array(1, 4, 5, 5, 8, 9)
    quickSort.sort(Array(6, 7, 3, 3, 0), randomSeed = Some(42)) shouldEqual Array(0, 3, 3, 6, 7)
  }

  "A quickSort" should "partition input array correctly" in {

    val partitionExamples = Table(
      ("arr", "leftIdx", "rightIdx", "pivotVal", "lowerIdx", "equalIdx", "scanIdx", "resArr", "resLowerIdx",
        "resEqualIdx", "resScanIdx"),
      (Array(4, 1, 5, 8, 9, 5), 0, 5, 4, 0, 0, 0, Array(1, 4, 5, 8, 9, 5), 1, 1, 5)
    )

    forAll(partitionExamples) {
      (arr: Array[Int], leftIdx: Int, rightIdx: Int, pivotVal: Int, lowerIdx: Int, equalIdx: Int,
       scanIdx: Int, resArr: Array[Int], resLowerIdx: Int, resEqualIdx: Int, resScanIdx: Int) =>
        val expPartitionState = PartitionState(resArr, resLowerIdx, resEqualIdx, resScanIdx)
        val res = quickSort.partition(arr, leftIdx, rightIdx, pivotVal, scanIdx, lowerIdx, equalIdx)
        res shouldEqual expPartitionState
    }
  }

  "A quickSort" should "update input array correctly for partitioning" in {
    val partitionUpdateExamples = Table(
      ("arr", "pivotVal", "scanIdx", "lowerIdx", "equalIdx", "resArr", "resLowerIdx", "resEqualIdx"),
      (Array(6, 4, 1, 5, 8, 9, 5), 6, 4, 3, 3, Array(6, 4, 1, 5, 8, 9, 5), 3, 3),
      (Array(6, 4, 1, 6, 6, 8, 9, 5, 3), 6, 7, 2, 4, Array(6, 4, 1, 5, 6, 6, 9, 8, 3), 3, 5)
    )

    forAll(partitionUpdateExamples) {
      (arr: Array[Int], pivotVal: Int, scanIdx:Int, lowerIdx: Int,
       equalIdx: Int, resArr: Array[Int], resLowerIdx: Int, resEqualIdx: Int) =>
        val partitionUpdateRes = quickSort.partitionUpdate(arr, pivotVal, scanIdx, lowerIdx, equalIdx)
        partitionUpdateRes.arr shouldEqual resArr
        partitionUpdateRes.lowerIdx shouldEqual resLowerIdx
        partitionUpdateRes.equalIdx shouldEqual resEqualIdx
    }
  }

}

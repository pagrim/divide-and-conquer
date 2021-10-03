import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import QuickSort.quickSort

class QuickSortSpec extends AnyFlatSpec with Matchers {

  "A quickSort" should "sort input array correctly" in {
    quickSort.sort(Array(4, 1, 5, 8, 9, 5), randomSeed = Some(42)) shouldEqual Array(1, 4, 5, 5, 8, 9)
    //quickSort.sort(Array(6, 7, 3, 3, 0), randomSeed = Some(42)) shouldEqual Array(0, 3, 3, 6, 7)
  }

  "A quickSort" should "partition input array correctly" in {
    val partition1 = quickSort.partition(Array(4, 1, 5, 8, 9, 5), 0, 3)
    partition1._1 shouldEqual Array(1, 4, 5, 8, 9, 5)
    partition1._2 shouldEqual 1
    val partition2 = quickSort.partition(Array(4, 1, 2, 3, 9, 5), 0, 3)
    partition2._1 shouldEqual Array(3, 1, 2, 4, 9, 5)
    partition2._2 shouldEqual 3
    val partition3 = quickSort.partition(Array(1, 4, 5, 5, 9, 8), 4, 5)
    partition3._1 shouldEqual Array(1, 4, 5, 5, 8, 9)
    partition3._2 shouldEqual 5
  }

  "A quickSort" should "update input array correctly for partitioning" in {
    val update1 = quickSort.partitionUpdate(Array(4, 1, 5, 8, 9, 5), pivotVal = 4, scanIdx = 4, lowerIdx = 2)
    update1._1 shouldEqual Array(4, 1, 5, 8, 9, 5)
    update1._2 shouldEqual 2
    val update2 = quickSort.partitionUpdate(Array(4, 1, 5, 8, 3, 5), pivotVal = 4, scanIdx = 4, lowerIdx = 1)
    update2._1 shouldEqual Array(4, 1, 3, 8, 5, 5)
    update2._2 shouldEqual 2
    val update3 = quickSort.partitionUpdate(Array(4, 1, 5, 8, 9, 5), pivotVal = 4, scanIdx = 1, lowerIdx = 0)
    update3._1 shouldEqual Array(4, 1, 5, 8, 9, 5)
    update3._2 shouldEqual 1
  }

}



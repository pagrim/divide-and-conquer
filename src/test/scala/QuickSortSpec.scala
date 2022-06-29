import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}


class QuickSortSpec extends AnyFlatSpec with Matchers {

  "A quickSort" should "sort input array correctly" in {

    val sortExamples = Table(("array", "randomSeed", "expRes"),
      (Array(4, 1, 5, 8, 9, 5), Some(42), Array(1, 4, 5, 5, 8, 9)),
      (Array(6, 7, 3, 3, 0), Some(42), Array(0, 3, 3, 6, 7)),
      (Array(2, 3, 9, 2, 9), Some(-941176648), Array(2, 2, 3, 9, 9))
    )

    forAll(sortExamples) {
      (array: Array[Int], randomSeed: Option[Int], expRes: Array[Int]) =>
        quickSort.sort(array, randomSeed = randomSeed)
        array shouldEqual expRes
    }
  }

  "A quickSort" should "partition input array correctly" in {

    val partitionExamples = Table(
      ("arr", "resArr", "j", "k"),
      (Array(1, 5, 8, 9, 5, 4), Array(1, 4, 8, 9, 5, 5), 0, 1),
      (Array(1, 5, 8, 6, 6, 9, 5, 6), Array(1, 5, 5, 6, 6, 6, 8, 9), 2, 5)
    )

    forAll(partitionExamples) {
      (arr: Array[Int], resArr: Array[Int], j: Int, k: Int) =>
        val res = quickSort.partition(arr = arr, left = 0, right = arr.length - 1)
        arr shouldEqual resArr
        res shouldEqual(j, k)
    }
  }
}
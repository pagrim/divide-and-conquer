import quickSort.partition
import com.typesafe.scalalogging.Logger
import scala.util.Random
import scala.util.control.Breaks.break

object quickSortPartitionVolTest {

  val logger: Logger = Logger("volumetest")

  def main(args: Array[String]): Unit = {
    Random.setSeed(42)
    for (i <- 0 to 1000) {
      val arrLen = Random.nextInt(20) + 1
      val input = Seq.fill(arrLen)(Random.nextInt(100)).toArray
      val original = input.map(identity)
      val pivot = original(0)
      val (j, k) = partition(input, left = 0, right = input.length - 1)
      try {
        if (arrLen > 1) {
          for (idx <- input.indices) {
            idx match {
              case x if x <= j => if (j > 0) assert(input(x) < pivot)
              case x if (x > j) & (x <= k) => assert(input(x) == pivot)
              case x if (x > k) => assert(input(x) > pivot)
            }
          }
        }
      } catch {
        case ae: AssertionError =>
          logger.info(s"Partition case not passed, index ${i}, starting array ${original}, ending array ${input}, j $j, k $k")
          break
      }
    }


  }
}

import quickSort.sort
import com.typesafe.scalalogging.Logger
import scala.util.Random
import scala.util.control.Breaks.break

object quickSortVolTest {

  val logger: Logger = Logger("volumetest")

  def main(args: Array[String]): Unit = {
    Random.setSeed(42)
    for(i <- 0 to 1000) {
      val arrLen = Random.nextInt(20) + 1
      val input = Seq.fill(arrLen)(Random.nextInt(100)).toArray
      val original = input.map(identity)
      sort(input)
      try {
      for(idx <- 1 until input.length){
        assert(input(idx) >= input(idx - 1))
        }
      } catch {
        case ae: AssertionError =>
          logger.info(s"Sorting incorrectly starting array ${original}, ending array ${input}")
          break
      }
    }

  }

}

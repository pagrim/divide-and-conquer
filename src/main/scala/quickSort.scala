import scala.io.StdIn.{readInt, readLine}
import scala.util.Random
import com.typesafe.scalalogging.Logger

object quickSort {

  val logger: Logger = Logger("quicksort")

  def sort(arr: Array[Int], randomSeed: Option[Int] = None): Unit = {
    /** *
     * This function runs the quicksort algorithm. It picks a random pivot and puts this at the rightmost position of
     * the array, then runs the partition algo to get the lower, mid and upper elements. The upper and lower parts
     * are then sorted to get the complete sort
     */
    val rdm = randomSeed match {
      case Some(i) => new Random(i)
      case _ =>
        val rdmGen = new Random()
        val seed = rdmGen.nextInt
        logger.info(s"Starting main sort using random seed ${seed}")
        new Random(seed)
      }
    _sort(arr, 0, arr.length - 1, rdm)
  }

  def _sort(arr: Array[Int], left: Int, right: Int, rdm: Random): Unit = {
    if (left < right) {
      val randIndex = rdm.nextInt(right - left) + left
      swap(arr, randIndex, right)
      val (j, k) = partition(arr, left, right)
      _sort(arr, left, j-1, rdm)
      _sort(arr, k+1, right, rdm)
    }
  }


  def partition(arr: Array[Int], left: Int, right: Int): (Int, Int) = {
    /** This function implements a partition on an array */
    val pivotVal = arr(right)
    var (j, k) = (left -1, left -1)
    for (i <- left until right -1) {
        arr(i) match {
         case x if x == pivotVal =>
           k += 1
           swap(arr, i, k)
         case x if x < pivotVal =>
           j += 1
           k += 1
           swap(arr, i, j)
           if(j!=k) swap(arr, i, k)
         case _ => ()
       }
    }
    k += 1
    swap(arr, k, arr.length - 1)
    (j, k)
  }

  def swap(arr: Array[Int], idx1: Int, idx2: Int): Unit = {
    val temp = arr(idx1)
    arr(idx1) = arr(idx2)
    arr(idx2) = temp
  }


  def main(args: Array[String]): Unit = {
    val numElems = readInt()
    val input = readLine().split(" ").map(el => el.toInt)
    assert(input.length == numElems)
    sort(input)
    print(input.mkString(" "))
  }

}

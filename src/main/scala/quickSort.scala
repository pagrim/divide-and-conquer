import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object quickSort {

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
        new Random(seed)
      }
    _sort(arr, 0, arr.length - 1, rdm)
  }

  def _sort(arr: Array[Int], left: Int, right: Int, rdm: Random): Unit = {
    if (left < right) {
      val randIndex = rdm.nextInt(right - left) + left
      swap(arr, randIndex, left)
      val (j, k) = partition(arr, left, right)
      _sort(arr, left, j, rdm)
      _sort(arr, k+1, right, rdm)
    }
  }


  def partition(arr: Array[Int], left: Int, right: Int): (Int, Int) = {
    /** This function implements a partition on an array */
    val pivotVal = arr(left)
    var (j, k) = (left, left)
    for (i <- left + 1 to right) {
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
    if(j> left) {
      swap(arr, j, left)
      j -= 1
    }
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

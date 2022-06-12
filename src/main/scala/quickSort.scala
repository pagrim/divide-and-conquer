import scala.io.StdIn.{readInt, readLine}

import scala.annotation.tailrec
import scala.util.Random
import com.typesafe.scalalogging.Logger

case class Indices(left: Int, right: Int, pivot: Int, lower: Int, equal: Int, scan: Int)

case class PartitionState(arr: Array[Int], lowerIdx: Int, equalIdx: Int, scanIdx: Int) {

  override def equals(other: Any): Boolean = other match {
    case ps: PartitionState =>
      ps.arr.sameElements(this.arr) && ps.lowerIdx == this.lowerIdx && ps.equalIdx == this.equalIdx &&
        ps.scanIdx == this.scanIdx
  }

  override def toString: String = {
    val arrRep = this.arr.mkString(sep = ",")
    s"arr:(${arrRep}),lowerIdx:${this.lowerIdx},equalIdx:${this.equalIdx},scanIdx:${this.scanIdx}"
  }
}

object quickSort {

  val logger: Logger = Logger("quicksort")

  def sort(input: Array[Int], randomSeed: Option[Int] = None): Array[Int] = {
    val rdm = randomSeed match {
      case Some(i) => new Random(i)
      case _ =>
        val rdmGen = new Random()
        val seed = rdmGen.nextInt
        logger.info(s"Using random seed ${seed}")
        new Random(seed)
      }
    _sort(input, 0, input.length, rdm, "root")
  }

  def _sort(input: Array[Int], leftIndex: Int, rightIndex: Int, randomGen: Random, desc: String): Array[Int] = {
    /** *
     * This function runs the quicksort algorithm. It picks a random pivot and puts this at the leftIndex position at
     * the start of the array, then runs the partition algo to get the position of the pivot. The upper and lower parts
     * are then sorted to get the complete sort
     * Uses the convention that if:
     * right == left => 0 elements
     * right = left + 1 => 1 element
     * right = left + 2 => 2 elements
     * right > left + 2 => 3 or more elements and run sorting
     * i.e. for the purpose of this function the rightIndex is exclusive
     */
    rightIndex match {
      case ri if(ri == leftIndex) => Array()
      case ri if(ri == leftIndex + 1) => Array(input(leftIndex))
      case ri if(ri == leftIndex + 2) =>
        if (input(rightIndex -1) >= input(leftIndex)) input.slice(leftIndex, rightIndex)
        else {
          Array(input(rightIndex-1), input(leftIndex))
        }
      case _ => {
        logger.debug(s"Starting main _sort on input: ${input}, range: ${desc}, leftIndex: ${leftIndex}, rightIndex: ${rightIndex}")
        val randIndex = randomGen.nextInt(rightIndex - leftIndex) + leftIndex
        val firstElement = input(leftIndex)
        val randPivotChosen = input.updated(leftIndex, input(randIndex)).updated(randIndex, firstElement)
        val partitionRes = partition(
          arr = randPivotChosen,
          Indices(
            left = leftIndex,
            right = rightIndex -1,
            pivot = leftIndex,
            lower = leftIndex,
            equal = leftIndex,
            scan = leftIndex + 1)
        )
        val lowerSorted = _sort(partitionRes.arr, leftIndex, partitionRes.lowerIdx + 1, randomGen, "lower")
        logger.debug(s"lowerSorted ${lowerSorted}")
        val equalSorted = partitionRes.arr.slice(math.min(partitionRes.lowerIdx + 1, partitionRes.equalIdx), partitionRes.equalIdx + 1)
        logger.debug(s"equalSorted: ${equalSorted}")
        val upperSorted = _sort(partitionRes.arr, math.min(partitionRes.equalIdx + 1, rightIndex -1), rightIndex, randomGen, "upper")
        logger.debug(s"upperSorted: ${upperSorted}")
        val concatSorted = lowerSorted ++ equalSorted ++ upperSorted
        logger.debug(s"concat: ${concatSorted}")
        concatSorted
      }
    }
  }

  @tailrec
  def partition(arr: Array[Int], indices: Indices): PartitionState = {
    /** *
     * This function implements updates on the partitioned part of the array until the left index reaches the right
     * index, then swaps the pivot to its final position and reduce the lower index in the case of a swap
     */
    val pivotVal = arr(indices.pivot)
    val updated = partitionUpdate(arr, pivotVal, indices.scan, indices.lower, indices.equal)
    if (indices.scan >= indices.right) {
      val swappedPivot = updated.arr.updated(indices.pivot, updated.arr(updated.lowerIdx)).updated(updated.lowerIdx, pivotVal)
      val lowerIdxAdjustment = if (indices.lower > indices.pivot) 1 else 0
      return PartitionState(swappedPivot, updated.lowerIdx - lowerIdxAdjustment, updated.equalIdx, updated.scanIdx)
    } else {
    }
    val newIndices = Indices(indices.left, indices.right, indices.pivot, updated.lowerIdx, updated.equalIdx, indices.scan + 1)
    partition(arr=updated.arr, newIndices)
  }

  def partitionUpdate(arr: Array[Int], pivotVal: Int, scanIdx: Int, lowerIdx: Int, equalIdx: Int): PartitionState = {
    /** *
     * Given a scanElement being considered this function returns the updated array after the positioning the scanElement
     * in the lower, equal or upper part of the array (the latter requiring no swaps since the scanElement is in this
     * by default)
     */
    val scanElement = arr(scanIdx)
    scanElement match {
      case x if x == pivotVal =>
        val newEqualIdx = equalIdx + 1
        val firstSwap = arr.updated(scanIdx, arr(newEqualIdx)).updated(newEqualIdx, scanElement)
        PartitionState(firstSwap, lowerIdx, newEqualIdx, scanIdx)
      case x if x < pivotVal =>
        val newLowerIdx = lowerIdx + 1
        val newEqualIdx = equalIdx + 1
        val firstSwap = arr.updated(scanIdx, arr(newLowerIdx)).updated(newLowerIdx, scanElement)
        val tempScanEl = firstSwap(scanIdx)
        val secondSwap = firstSwap.updated(scanIdx, firstSwap(newEqualIdx)).updated(newEqualIdx, tempScanEl)
        PartitionState(secondSwap, newLowerIdx, newEqualIdx, scanIdx)
      case _ =>
        PartitionState(arr, lowerIdx, equalIdx, scanIdx)
    }
  }

  def main(args: Array[String]): Unit = {
    val numElems = readInt()
    val input = readLine().split(" ").map(el => el.toInt)
    assert(input.length == numElems)
    print(sort(input).mkString(" "))
  }

}

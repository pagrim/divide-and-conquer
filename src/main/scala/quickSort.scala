import scala.io.StdIn.{readInt, readLine}

import scala.annotation.tailrec
import scala.util.Random

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

  def sort(input: Array[Int], randomSeed: Option[Int] = None): Array[Int] = {
    val rdm = randomSeed match {
      case Some(i) => new Random(i)
      case _ => new Random()
    }
    _sort(input, 0, input.length - 1, rdm)
  }

  def _sort(input: Array[Int], leftIndex: Int, rightIndex: Int, randomGen: Random): Array[Int] = {
    /** *
     * This function runs the quicksort algorithm. It picks a random pivot and puts this at the leftIndex position at
     * the start of the array, then runs the partition algo to get the position of the pivot. The upper and lower parts
     * are then sorted to get the complete sort
     */
    if (leftIndex >= rightIndex) {
      try {
        Array(input(leftIndex))
      } catch {
        case e: ArrayIndexOutOfBoundsException => Array()
      }
    } else {
      val randIndex = randomGen.nextInt(rightIndex - leftIndex) + leftIndex
      val firstElement = input(leftIndex)
      val randPivotChosen = input.updated(leftIndex, input(randIndex)).updated(randIndex, firstElement)
      val partitionRes = partition(
        arr = randPivotChosen,
        pivotIdx = leftIndex,
        lowerIdx = leftIndex,
        equalIdx = leftIndex,
        scanIdx = leftIndex + 1)

      val lowerSorted = _sort(partitionRes.arr, leftIndex, partitionRes.lowerIdx, randomGen)
      val upperSorted = _sort(partitionRes.arr, partitionRes.equalIdx + 1, rightIndex, randomGen)
      val equalSorted = partitionRes.arr.slice(partitionRes.lowerIdx + 1, partitionRes.equalIdx + 1)
      lowerSorted ++ equalSorted ++ upperSorted
    }
  }

  @tailrec
  def partition(arr: Array[Int], pivotIdx: Int, lowerIdx: Int, equalIdx: Int, scanIdx: Int): PartitionState = {
    /** *
     * This function implements updates on the partitioned part of the array until the left index reaches the right
     * index, then swaps the pivot to its final position and reduce the lower index in the case of a swap
     */
    val pivotVal = arr(pivotIdx)
    val updated = partitionUpdate(arr, pivotVal, scanIdx, lowerIdx, equalIdx)
    if (scanIdx >= arr.length - 1) {
      val swappedPivot = updated.arr.updated(pivotIdx, updated.arr(updated.lowerIdx)).updated(updated.lowerIdx, pivotVal)
      val lowerIdxAdjustment = if (lowerIdx > pivotIdx) 1 else 0
      return PartitionState(swappedPivot, updated.lowerIdx - lowerIdxAdjustment, updated.equalIdx, updated.scanIdx)
    } else {
    }
    partition(arr=updated.arr, pivotIdx=pivotIdx, lowerIdx=updated.lowerIdx, equalIdx=updated.equalIdx, scanIdx=scanIdx + 1)
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
        val secondSwap = firstSwap.updated(scanIdx, arr(newEqualIdx)).updated(newEqualIdx, tempScanEl)
        PartitionState(secondSwap, newLowerIdx, newEqualIdx, scanIdx)
      case _ =>
        PartitionState(arr, lowerIdx, equalIdx, scanIdx)
    }
  }

  def main(args: Array[String]): Unit = {
    val numElems = readInt()
    val input = readLine().split(" ").map(el => el.toInt)
    assert(input.length == numElems)
    print(sort(input))
  }

}

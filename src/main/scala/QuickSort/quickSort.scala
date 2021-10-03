package QuickSort

import scala.annotation.tailrec
import scala.util.Random

object quickSort {

  def sort(input: Array[Int], randomSeed: Option[Int] = None): Array[Int] = {
    val rdm = randomSeed match {
      case Some(i) => new Random(i)
      case _ => new Random()
    }
    _sort(input, 0, input.length - 1, rdm)
  }

  def _sort(input: Array[Int], leftIndex: Int, rightIndex: Int, randomGen: Random): Array[Int] = {
    if(leftIndex >= rightIndex){
      Array(input(leftIndex))
    } else {
      val randIndex = randomGen.nextInt(rightIndex - leftIndex) + leftIndex
      val firstElement = input(leftIndex)
      val randPivotInput = input.updated(leftIndex, input(randIndex)).updated(randIndex, firstElement)
      val partitionArrayIndex = partition(randPivotInput, leftIndex, rightIndex)
      val lowerSorted = _sort(partitionArrayIndex._1, leftIndex, partitionArrayIndex._2 - 1, randomGen)
      val upperSorted =_sort(partitionArrayIndex._1, partitionArrayIndex._2 + 1, rightIndex, randomGen)
      lowerSorted ++ Array(partitionArrayIndex._1(partitionArrayIndex._2)) ++ upperSorted
    }
  }

  def partition(input: Array[Int], leftIdx: Int, rightIdx: Int): (Array[Int], Int) = {
    _partition(input, leftIdx, rightIdx, input(leftIdx), leftIdx + 1, leftIdx)
  }

  @tailrec
  def _partition(input: Array[Int], leftIdx: Int, rightIdx: Int, pivotVal:Int, scanIdx: Int, lowerIdx: Int): (Array[Int], Int) = {
    val newInputLowerIndex = partitionUpdate(input, pivotVal, scanIdx, lowerIdx)
    if(scanIdx >= rightIdx) {
      val finalInput = newInputLowerIndex._1.updated(leftIdx, newInputLowerIndex._1(newInputLowerIndex._2)).updated(newInputLowerIndex._2, pivotVal)
      return (finalInput, newInputLowerIndex._2)
    } else {
    }
    _partition(newInputLowerIndex._1, leftIdx, rightIdx, pivotVal, scanIdx + 1, newInputLowerIndex._2)
  }

  def partitionUpdate(input: Array[Int], pivotVal: Int, scanIdx: Int, lowerIdx: Int): (Array[Int], Int) = {
      val scanElement = input(scanIdx)
      if (scanElement <= pivotVal) {
        val newLowerIdx = lowerIdx + 1
        (input.updated(scanIdx, input(newLowerIdx)).updated(newLowerIdx, scanElement), newLowerIdx)
      } else {
        (input, lowerIdx)
      }
    }

}

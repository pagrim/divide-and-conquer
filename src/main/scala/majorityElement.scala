import scala.io.StdIn.{readInt, readLine}

object majorityElement {

  def getMajority(input: Array[Long]): Long = {
    val majority = _getMajority(input, 0, input.length -1)
    val majorityCount = countFrequency(input, 0, input.length -1, majority)
    if(majorityCount > input.length/2){
    majority} else {
      0
    }
  }

  def _getMajority(input: Array[Long], leftIndex: Int, rightIndex: Int): Long = {

    if( leftIndex == rightIndex) {
      input(leftIndex)
    } else {

      val midPoint = (leftIndex + rightIndex)/2

      val leftMajority = _getMajority(input, 0, midPoint)
      val rightMajority = _getMajority(input, midPoint + 1, rightIndex)

      val leftFreq = countFrequency(input, leftIndex, rightIndex, leftMajority)
      val rightFreq = countFrequency(input, leftIndex, rightIndex, rightMajority)

      if(leftFreq > rightFreq){
        leftMajority
      } else {
        rightMajority
      }

    }

  }

  def countFrequency(input: Array[Long], leftIndex: Int, rightIndex: Int, target: Long): Int = {
    input.slice(leftIndex, rightIndex +1).count( x => x == target)
  }

  def main(args: Array[String]): Unit = {
    val numElems = readInt()
    val input = readLine().split(" ").map(el => el.toLong)
    assert(input.length == numElems)
    print(getMajority(input))
  }


}



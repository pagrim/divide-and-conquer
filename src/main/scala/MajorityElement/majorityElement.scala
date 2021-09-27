package MajorityElement
import com.typesafe.scalalogging.Logger

object majorityElement {

  val logger: Logger = Logger("MajorityElementLogger")

  def hasMajorityElement(input: Array[Int]): Int = {
    val majorityElement = _hasMajorityElement(input)
    if(majorityElement._1 && checkMajority(input, majorityElement._2)) {
      logger.info(s"Found majority element ${majorityElement._2}"); 1
    } else 0
  }

  def checkMajority(input: Array[Int], checkVal: Int): Boolean = {
    logger.info(s"Checking whether value ${checkVal} is a majority for array ${input}")

    val checkCount = input.count( x => x == checkVal)
    checkCount > input.length/2
  }

  def _hasMajorityElement(input: Array[Int]): (Boolean, Int) = {
    logger.info(s"Finding majority element for array ${input}")
    if(input.length <= 3) {
      directMajority(input)
    } else {
      val midPoint = math.floor(input.length/2).toInt
      logger.info(s"Midpoint index is ${midPoint}")
      val lowerMajority = _hasMajorityElement(input.slice(0, midPoint))
      if(lowerMajority._1){ lowerMajority } else {
        val upperMajority = _hasMajorityElement(input.slice(midPoint, input.length))
        if(upperMajority._1){
          upperMajority } else {
          (false, -1)
      }
      }
    }
  }

  def directMajority(input: Array[Int]): (Boolean, Int) = {
    val counts = input.groupBy(identity).map(x => (x._1, x._2.length))
    val maxCount = counts.values.max
    (maxCount > input.length/2, maxCount)
    }

}



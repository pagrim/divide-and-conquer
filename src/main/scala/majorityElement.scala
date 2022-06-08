import scala.io.StdIn.{readInt, readLine}
import scala.annotation.tailrec

object majorityElement {

  def getMajority(input: Array[Long]): (Long, Int) = {
    val majorityThresh = input.length/2
    val counts = input.groupBy(identity).map(grp => (grp._1, grp._2.length)).toArray
    fetchMajority(counts, 0, majorityThresh)
  }

  @tailrec
  def fetchMajority(counts: Array[(Long, Int)], idx: Int, majorityThresh: Int): (Long, Int) = {
    if (idx == counts.length) { (-1L, 0) }
    else if (counts(idx)._2 >= majorityThresh) {
      (counts(idx)._1, 1)
    } else {
      fetchMajority(counts, idx + 1, majorityThresh)
    }
}


  def main(args: Array[String]): Unit = {
    val numElems = readInt()
    val input = readLine().split(" ").map(el => el.toLong)
    assert(input.length == numElems)
    val (_, flag) = getMajority(input)
    print(flag)
  }


}



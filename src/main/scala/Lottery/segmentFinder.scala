package Lottery

import scala.annotation.tailrec

object segmentFinder {

  //TODO add in method for sorting, currently assuming points and segments are sorted

  def countContainingSegments(points: Array[Int], segments: Array[Segment]): Array[Int] = {
    val initAcc = Accumulator(0, Array(), Array())
    val finalAcc = points.foldLeft( initAcc )( (acc, pt) => accumulate(acc, pt, segments) )
    finalAcc.counts
  }

  def accumulate(acc: Accumulator, targetPoint: Int, segments: Array[Segment]): Accumulator = {

    val newLeftIndex = findSegmentsLeftIndex(acc.prevLeftIdx, targetPoint, segments)
    val bufferWithLefts = acc.buffer ++ segments.slice(acc.prevLeftIdx, newLeftIndex)
    val filteredBuffer = bufferWithLefts.filter( seg => seg.right >= targetPoint)
    val count = filteredBuffer.length
    val newCounts = acc.counts ++ Array(count)
    Accumulator(newLeftIndex, newCounts, filteredBuffer)
  }


  @tailrec
  def findSegmentsLeftIndex(index: Int, targetPoint: Int, segments: Array[Segment]): Int = {
    if(index == segments.length || segments(index).left >= targetPoint){
      index
    } else {
      findSegmentsLeftIndex(index + 1, targetPoint, segments)
    }
  }

}

case class Segment(left: Int, right: Int)
case class Accumulator(prevLeftIdx: Int, counts: Array[Int], buffer: Array[Segment])

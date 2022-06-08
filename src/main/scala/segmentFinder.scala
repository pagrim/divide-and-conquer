import scala.io.StdIn.readLine

case class Segment(left: Int, right: Int)
case class Accumulator(leftPts: Int, rightPts: Int, counts: Map[Int, Int])

sealed trait PointMarker {def pos: Int}

object PointMarker {
  case class Left(pos: Int) extends PointMarker
  case class Point(pos: Int) extends PointMarker
  case class Right(pos: Int) extends PointMarker
}

object segmentFinder {

  def countContainingSegments(points: Array[Int], segments: Array[Segment]): Array[Int] = {
    /** *
     * This method creates an ordered array of PointMarkers and computes how many segment contain each point by
     * calculating the number of Left PointMarkets minus the number of Right PointMarkers when a PointMarker.Point
     * is reached
     */
    val positions = points.map(pt => PointMarker.Point(pt)) ++
      segments.flatMap(seg => Array(PointMarker.Left(seg.left), PointMarker.Right(seg.right)))
    val positionsOrdered = positions.sortBy( pstn => pointOrder(pstn))
    val finalAcc = positionsOrdered.foldLeft(Accumulator(0, 0, Map()))(
      (acc, pt) => pt match {
        case _: PointMarker.Left => Accumulator(acc.leftPts + 1, acc.rightPts, acc.counts)
        case _: PointMarker.Right => Accumulator(acc.leftPts, acc.rightPts + 1, acc.counts)
        case pt: PointMarker.Point =>
          val netPts = acc.leftPts - acc.rightPts
          Accumulator(acc.leftPts, acc.rightPts, acc.counts ++ Map(pt.pos -> netPts))
      })
    points.map(pt => finalAcc.counts(pt))
  }

  def pointOrder(pnt: PointMarker): (Int, Int) = {
    pnt match {
      case lft: PointMarker.Left => (lft.pos, 0)
      case pnt: PointMarker.Point => (pnt.pos, 1)
      case rgt: PointMarker.Right => (rgt.pos, 2)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = readLine().split(" ").map(el => el.toInt)
    val numSegments = input(0)
    val numPoints = input(1)
    val segInputs = for (_ <- 1 to numSegments) yield readLine()

    val segments = segInputs.map(inp => inp.split(" ").map(el => el.toInt)).map(
      segArr => Segment(segArr(0), segArr(1))).toArray
    val points = readLine().split(" ").map(el => el.toInt)
    assert(points.length == numPoints)

    print(countContainingSegments(points, segments).mkString(" "))

  }
}


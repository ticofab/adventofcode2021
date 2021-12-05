package day05

import scala.collection.mutable
import scala.io.Source

@main
def day05() = {
  val cloudGrid = parseInput("day05input.clouds")
  println(s"There are ${cloudGrid.dangerousPoints()} dangerous points in the cloud.")
  println(s"the most dangerous point is ${cloudGrid.mostDangerousPoint()}")
}

def parseInput(resourceName: String): CloudGrid = {
  val lines = Source.fromResource(resourceName).getLines.toList
  val segments = lines.map(s => s.split("->")
      .toVector
      .map(_.trim)
      .map(coordinatesStr => coordinatesStr.split(',').toVector)
      .map(coordinates => Point(coordinates.head.toInt, coordinates.last.toInt)))
  val filteredSegments = segments.filter(isLineOrColumn)
  val allPointsCoveredByClouds = filteredSegments
    .map(twoPoints => {
      val point1 = twoPoints.head
      val point2 = twoPoints.last
      if (point1.x == point2.x)
        // column
        if (point1.y < point2.y) columnGenerator(point1.x, point1.y, point2.y)
        else columnGenerator(point1.x, point2.y, point1.y)
      else
        // line
        if (point1.x < point2.x) lineGenerator(point1.y, point1.x, point2.x)
        else lineGenerator(point1.y, point2.x, point1.x)
    })
  val allPointsFlattened = for {
    pointSequence <- allPointsCoveredByClouds
    point <- pointSequence
  } yield point
  val cloudGrid = mutable.Map[Point, Int]().empty
  var largestX = 0
  var largestY = 0
  allPointsFlattened.foreach(point => {
    if (point.x > largestX) largestX = point.x
    if (point.y > largestY) largestY = point.y
    cloudGrid.get(point) match {
      case Some(currentPointScore) =>
        val newPointScore = currentPointScore + 1
        cloudGrid(point) = newPointScore
      case None => cloudGrid += (point -> 1)
    }
  })
  CloudGrid(cloudGrid.toMap, largestX, largestY)
}

def isLineOrColumn(twoPoints: Vector[Point]) =
  twoPoints.head.x == twoPoints.last.x || twoPoints.head.y == twoPoints.last.y

case class Point(x: Int, y: Int)

case class CloudGrid(grid: Map[Point, Int], width: Int, height: Int) {
  def printGrid() =
    for(y <- Range.inclusive(0, height)) {
      for (x <- Range.inclusive(0, width)) {
        print(grid.getOrElse(Point(x, y), 0))
        print(" ")
      }
      println
    }

  def dangerousPoints() = grid.count((point, score) => score > 1)

  def mostDangerousPoint(): (Point, Int) = grid maxBy((point, score) => score)
}

def lineGenerator(fixedY: Int, start: Int, end: Int) =
  for i <- Range.inclusive(start, end) yield Point(i, fixedY)

def columnGenerator(fixedX: Int, start: Int, end: Int) =
  for i <- Range.inclusive(start, end) yield Point(fixedX, i)


// I want to generate a map Point -> Score
// For each line covering a set of points, each point increases its score by 1.
// I need a function that given two points, returns a list of points between. So if I read
//   9,4 -> 3,4
// then pointsBetween(Point(9,4), Point(3,4)) should return
//   List(Point(9,4), Point(8, 4), Point(7, 4), Point(6, 4), Point(5, 4), Point(4, 4), Point(3, 4))
// Then from all these lists I can populate my map.
//
// 1. identify whether it's a line or a column:
//    if (point1.x == point2.x) column
//    else if (point1.y == point2.y) line
//    else skip for part 1
//
// 2. find the biggest value:
//    if (line)
//      if (point2.y > point1.y)
//         decreasing [start == point2.x, step == -1, ]
//      else
//         increasing [start == point1.y, step == 1]
//    else
//      if (point2.x > point1.x)
//         decreasing [start == point2.x, step == -1]
//      else
//         increasing [start == point1.x, step == 1]
//
// 3. I generate a Range for these two points
//
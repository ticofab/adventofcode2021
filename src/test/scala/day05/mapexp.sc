import scala.collection.mutable

case class Point(x: Int, y: Int)
var cloudGrid = Map[Point, Int]()
val point = Point(9, 4)
val point2 = Point(3, 5)
cloudGrid = cloudGrid + (point -> 8)
cloudGrid = cloudGrid + (point2 -> 5)
cloudGrid.get(point2)
val m2 = mutable.Map[Point, Int]()
m2 += (point2 -> 4)
m2 += (point -> 5)
m2(point)
m2(point) = 6
m2
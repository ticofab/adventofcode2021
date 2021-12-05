case class Point(x: Int, y: Int)
val r1 = Range(1, 5)
val r2 = Range(9, 6)
val r3 = Range(9, 6, -1)

def lineGenerator(fixedY: Int, start: Int, end: Int, step: Int) =
  for i <- Range.inclusive(start, end, step) yield Point(i, fixedY)

def columnGenerator(fixedX: Int, start: Int, end: Int, step: Int) =
  for i <- Range.inclusive(start, end, step) yield Point(fixedX, i)

// 9,4 -> 3,4
var line = lineGenerator(4, 9, 3, -1)

// 7,0 -> 7,4
var column = columnGenerator(7, 0, 4, 1)
package day02

case class Position(distance: Int, depth: Int, aim: Int) {

  def withDepth(depthDelta: Int) = this.copy(aim = aim + depthDelta)

  def withDistance(distanceDelta: Int) = this.copy(
    distance = distance + distanceDelta,
    depth = depth + (aim * distanceDelta))

}


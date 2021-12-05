package day02

import org.scalatest.wordspec.AnyWordSpec
import Directions.*

class Day02test extends AnyWordSpec {
  "The Delta extraction function" should {
    "extract deltas correctly" in {
      val moves = List(Move(Directions.forward, 3), Move(Directions.forward, 5))
      val deltas = extractCumulativeDeltas(moves)
      assert(deltas(Directions.forward) == 8)

      val moves2 = List(Move(Directions.forward, 3), Move(Directions.forward, -4))
      val deltas2 = extractCumulativeDeltas(moves2)
      assert(deltas2(Directions.forward) == -1)

      val moves3 = List(
        Move(Directions.forward, 3),
        Move(Directions.forward, 11),
        Move(Directions.down, 4),
        Move(Directions.up, 7),
        Move(Directions.down, 1))
      val deltas3 = extractCumulativeDeltas(moves3)
      assert(deltas3(Directions.forward) == 14)
      assert(deltas3(Directions.down) == 5)
      assert(deltas3(Directions.up) == 7)
    }
  }

  "The position calculation" should {
    "change correctly" in {
      val moves = List(
        Move(forward, 5),
        Move(down, 5),
        Move(forward, 8),
        Move(up, 3),
        Move(down, 8),
        Move(forward, 2)
      )
      val finalPosition = evaluateFinalPositionStartingFromZero(moves)
      assert(finalPosition.depth * finalPosition.distance == 900)
    }
  }
}

package day02

import day02.Directions.*
import day02.Move.*

import scala.io.Source

@main
def main(): Unit = {
  val moves = Source.fromResource("day02input.moves")
    .getLines()
    .map(s => s.split(' ').toList)
    .map(ss => Move(Directions.valueOf(ss.head), ss.last.toInt))
    .toList

  val finalPosition = evaluateFinalPositionStartingFromZero(moves)

  println(finalPosition.distance * finalPosition.depth)
}

def extractCumulativeDeltas(moves: List[Move]): Map[Directions, Int] =
  moves
    .groupBy(_.direction)
    .view.mapValues(_.foldLeft(0)((acc, move) => acc + move.steps)).toMap

def evaluateFinalPositionStartingFromZero(moves: List[Move]) = {
  moves.foldLeft(Position(0, 0, 0))((position, move) => move.direction match {
    case Directions.forward => position.withDistance(move.steps)
    case Directions.down => position.withDepth(move.steps)
    case Directions.up => position.withDepth(move.steps * -1)
  })
}
package day06

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

@main
def day06() = {
  val initialState: List[Int] = parseInputState("day06input.lanternfish")
  val finalState = evolveState(initialState, 80)
  val finalAmountOfFish = observeAmountOfFish(finalState)
  println(s"After 80 days, we have $finalAmountOfFish fish.")

  val initialStateBetter = mutable.Map[Int, BigInt](
    0 -> BigInt(initialState.count(_ == 0)),
    1 -> BigInt(initialState.count(_ == 1)),
    2 -> BigInt(initialState.count(_ == 2)),
    3 -> BigInt(initialState.count(_ == 3)),
    4 -> BigInt(initialState.count(_ == 4)),
    5 -> BigInt(initialState.count(_ == 5)),
    6 -> BigInt(initialState.count(_ == 6)),
    7 -> BigInt(initialState.count(_ == 7)),
    8 -> BigInt(initialState.count(_ == 8))
  )
  val finalStatePart2 = evolveStateBetter(initialStateBetter, 256)
  val finalAmountOfFishPart2 = getAmountOfFish(finalStatePart2.toMap)
  println(s"After 256 days, we have $finalAmountOfFishPart2 fish.")
}

@tailrec
def evolveState(state: List[Int], days: Int): List[Int] = {

  println(s"days left: $days")
  days match {
    case 0 => state
    case _ =>
      // Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each
      // other number decreases by 1 if it was present at the start of the day.

      val newFish = state.count(_ == 0)

      val evolvedState = state.map {
        case 0 => 6
        case i => i - 1
      }

      val finalStateForToday = evolvedState ::: List.fill(newFish)(8)
      evolveState(finalStateForToday, days - 1)
  }
}

@tailrec
def evolveStateBetter(state: mutable.Map[Int, BigInt], days: Int): mutable.Map[Int, BigInt] = {
  println(s"days left: $days")
  days match {
    case 0 => state
    case _ =>
      // Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each
      // other number decreases by 1 if it was present at the start of the day.
      val zeroes = state.getOrElse(0, BigInt(0))
      val ones = state.getOrElse(1, BigInt(0))
      val twos = state.getOrElse(2, BigInt(0))
      val threes = state.getOrElse(3, BigInt(0))
      val fours = state.getOrElse(4, BigInt(0))
      val fives = state.getOrElse(5, BigInt(0))
      val sixes = state.getOrElse(6, BigInt(0))
      val sevens = state.getOrElse(7, BigInt(0))
      val eights = state.getOrElse(8, BigInt(0))
      state(0) = ones
      state(1) = twos
      state(2) = threes
      state(3) = fours
      state(4) = fives
      state(5) = sixes
      state(6) = zeroes + sevens
      state(7) = eights
      state(8) = zeroes
      evolveStateBetter(state, days - 1)
  }
}

def getAmountOfFish(state: Map[Int, BigInt]): BigInt = state.foldLeft[BigInt](BigInt(0)){ case (acc, (key, value)) => acc + value }
def observeAmountOfFish(state: List[Int]) = state.size

def parseInputState(resourceName: String): List[Int] =
  Source.fromResource(resourceName).getLines().toList.head.split(',').map(_.toInt).toList


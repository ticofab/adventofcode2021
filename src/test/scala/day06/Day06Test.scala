package day06

import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class Day06Test extends AnyWordSpec {
  "A pack of lantern fish" should {

    "evolve correctly" in {
      val initialState = parseInputState("day06input.test.lanternfish")
      val intermediateState = evolveState(initialState, 18)
      val intermediateAmountOfFish = observeAmountOfFish(intermediateState)
      assert(intermediateAmountOfFish == 26)
      val finalState = evolveState(intermediateState, 80 - 18)
      val finalAmountOfFish = observeAmountOfFish(finalState)
      assert(finalAmountOfFish == 5934)
    }

    "evolve correctly and quickly too" in {
      val initialState = mutable.Map(1 -> BigInt(1), 2 -> BigInt(1), 3 -> BigInt(2), 4 -> BigInt(1))
      val intermediateState = evolveStateBetter(initialState, 18)
      val intermediateAmountOfFish = getAmountOfFish(intermediateState.toMap)
      assert(intermediateAmountOfFish == 26)
      val finalState = evolveStateBetter(intermediateState, 80 - 18)
      val finalAmountOfFish = getAmountOfFish(finalState.toMap)
      assert(finalAmountOfFish == 5934)
      val extremeLongFinalState = evolveStateBetter(initialState, 256)
      assert(getAmountOfFish(extremeLongFinalState.toMap) == BigInt(26984457539L))
    }
  }
}

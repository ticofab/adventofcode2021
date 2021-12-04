package day04

import org.scalatest.wordspec.AnyWordSpec

class Day04Test extends AnyWordSpec {

  "A winning board finder" should {
    "Find the winning board" in {
      val bingoInput = parseInput("day04input.bingo.test")
      val winningBoardAndNumber = findWinningBoard(bingoInput)
      val boardScore = getBoardScore(winningBoardAndNumber)
      assert(boardScore == 4512)
    }
  }
}

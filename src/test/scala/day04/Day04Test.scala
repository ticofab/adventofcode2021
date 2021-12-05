package day04

import org.scalatest.wordspec.AnyWordSpec

class Day04Test extends AnyWordSpec {

  "A board finder" should {

    val bingoInput = parseInput("day04input.bingo.test")

    "Find the winning board" in {
      val winningBoardAndNumber = findWinningBoard(bingoInput)
      val boardScore = getBoardScore(winningBoardAndNumber)
      assert(boardScore == 4512)
    }

    "Find the last winning board" in {
      val lastBoardAndNumber = findLastWinningBoard(bingoInput)
      val score = getBoardScore(lastBoardAndNumber)
      assert(score == 1924)
    }
  }
}

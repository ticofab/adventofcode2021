package day04

import scala.annotation.tailrec
import scala.io.Source

@main
def main() = {
  val bingoInput = parseInput("day04input.bingo")
  println(s"parsed ${bingoInput.boards.length} boards and ${bingoInput.extractedNumbers.length} extractions.")
  val winningBoardAndWinningNumber = findWinningBoard(bingoInput)
  val score = getBoardScore(winningBoardAndWinningNumber)
  println(s"The score of the winning board is $score and the winning number was ${winningBoardAndWinningNumber.number}")

  val losingBoardAndLastWinningNumber = findLastWinningBoard(bingoInput)
  val score2 = getBoardScore(losingBoardAndLastWinningNumber)
  println(s"The score of the last winning board is $score2 and the last winning number was ${losingBoardAndLastWinningNumber.number}")
}

def printBoardLine(line: List[BoardNumber]) =
  line.map(bn => if (bn.isMarked) "xx" else bn.number.toString).mkString(" ")

// Side effecting function that returns a modified version of the parameter.
def markBoardsWithNumber(boards: List[Board], n: Int) = {
  boards.foreach(
    _.lines.foreach(
      _.foreach(bn => if (bn.number == n) bn.mark())))
  boards
}

// returns true if the given board is a winning one
def isWinningBoard(board: Board): Boolean = {
  val isLineBingo = board.lines.exists(_.forall(_.isMarked))

  // pure brutality
  var isColumnBingo = false
  for (i <- 0 until 5) {
    val column = for {
      line <- board.lines
    } yield line(i)
    isColumnBingo = isColumnBingo || column.forall(_.isMarked)
  }

  isColumnBingo || isLineBingo
}

def findLastWinningBoard(bingoInput: BingoInput): SelectedBoardAndNumber = {

  @tailrec
  def removeWinningBoardRec(remainingBoards: List[Board], remainingNumbers: List[Int]): SelectedBoardAndNumber = {
    remainingNumbers match {
      case n :: tailn =>
        val markedBoards = markBoardsWithNumber(remainingBoards, n)
        markedBoards match {
          case lastBoard :: Nil =>
            if isWinningBoard(lastBoard) then SelectedBoardAndNumber(lastBoard, n)
            else removeWinningBoardRec(markedBoards, tailn)
          case oneBoard :: otherBoards =>
            val updatedRemainingBoards = markedBoards.filterNot(isWinningBoard)
            removeWinningBoardRec(updatedRemainingBoards, tailn)
          case Nil => throw Exception("maybe two boards were losing at the same time")
        }
      case Nil => throw Exception("no losing boards!")
    }
  }

  removeWinningBoardRec(bingoInput.boards, bingoInput.extractedNumbers)
}

def findWinningBoard(bingoInput: BingoInput): SelectedBoardAndNumber = {

  @tailrec
  def findWinningBoardRec(markedBoardsSoFar: List[Board], remainingNumbers: List[Int]): SelectedBoardAndNumber = {
    remainingNumbers match {
      case head :: tail =>
        val markedBoards = markBoardsWithNumber(markedBoardsSoFar, head)
        markedBoards.find(board => isWinningBoard(board)) match {
          case Some(winningBoard) => SelectedBoardAndNumber(winningBoard, head)
          case None => findWinningBoardRec(markedBoards, tail)
        }
      case Nil => throw Exception("No winning board")
    }
  }

  findWinningBoardRec(bingoInput.boards, bingoInput.extractedNumbers)
}

def parseInput(resourceName: String): BingoInput = {

  def parseBoard(input: List[String]): Board = {
    val numbers = for {
      line <- input
      if line.nonEmpty
      numbers = line.split(' ').filterNot(_.isBlank).map(s => BoardNumber(s.trim.toInt))
    } yield numbers.toList
    Board(numbers)
  }

  @tailrec
  def parseBoards(boards: List[Board], input: List[String]): List[Board] = {
    input match {
      case Nil => boards
      case head :: Nil => boards
      case head :: "" :: Nil => boards
      case head :: tail =>
        // this is the beginning of a new board
        val parsedBoard = parseBoard(input.slice(1, 6))
        parseBoards(parsedBoard :: boards, input.drop(6))
    }
  }

  val lines = Source.fromResource(resourceName).getLines.toList
  val extractedNumbers = lines.head.split(',').map(_.toInt).toList
  val boards = parseBoards(List(), lines.tail).reverse
  BingoInput(extractedNumbers, boards)
}

def getBoardScore(selectedBoard: SelectedBoardAndNumber) = {
  println(s"the selected number is ${selectedBoard.number} and the selected board is:")
  selectedBoard.board.lines.foreach(l => println(printBoardLine(l)))
  val nonMarkedNumbers = for {
    line <- selectedBoard.board.lines
    boardNumber <- line
    if !boardNumber.isMarked
  } yield boardNumber
  nonMarkedNumbers.map(_.number).sum * selectedBoard.number
}

case class BoardNumber(number: Int, var isMarked: Boolean = false) {
  def mark() = this.isMarked = true
}

case class Board(lines: List[List[BoardNumber]])

case class BingoInput(extractedNumbers: List[Int], boards: List[Board])

case class SelectedBoardAndNumber(board: Board, number: Int)

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
}

def getBoardScore(winningBoardAndWinningNumber: WinningBoardAndWinningNumber) = {
  println("the winning board is:")
  winningBoardAndWinningNumber.board.lines.foreach(l => println(printLine(l)))
  val nonMarkedNumbers = for {
    line <- winningBoardAndWinningNumber.board.lines
    boardNumber <- line
    if !boardNumber.isMarked
  } yield boardNumber
  nonMarkedNumbers.map(_.number).sum * winningBoardAndWinningNumber.number
}

def printLine(line: List[BoardNumber]) = {
  line.map(bn => if (bn.isMarked) "xx" else bn.number.toString).mkString(" ")
}

def findWinningBoard(bingoInput: BingoInput): WinningBoardAndWinningNumber = {

  def markBoardsWithNumber(boards: List[Board], n: Int) = {
    boards.foreach(
      _.lines.foreach(
        _.foreach(bn => if (bn.number == n) bn.mark())))
    boards
  }

  def findWinningBoard(boards: List[Board]): Option[Board] =
    boards.find(board => {
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

    })

  @tailrec
  def findWinningBoardRec(markedBoardsSoFar: List[Board], remainingNumbers: List[Int]): WinningBoardAndWinningNumber = {
    remainingNumbers match {
      case head :: tail =>
        val markedBoards = markBoardsWithNumber(markedBoardsSoFar, head)
        findWinningBoard(markedBoards) match {
          case Some(winningBoard) => WinningBoardAndWinningNumber(winningBoard, head)
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

case class BoardNumber(number: Int, var isMarked: Boolean = false) {
  def mark() = this.isMarked = true
}

case class Board(lines: List[List[BoardNumber]])

case class BingoInput(extractedNumbers: List[Int], boards: List[Board])

case class WinningBoardAndWinningNumber(board: Board, number: Int)
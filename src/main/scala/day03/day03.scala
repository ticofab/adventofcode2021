package day03

import day02.{Directions, Move}

import scala.annotation.tailrec
import scala.io.Source

@main
def main() = {
  val readingsStr = Source.fromResource("day03input.rates").getLines.toList
  val readings = parseDiagnosticReport(readingsStr)
  val rates = RateExtractor.extractRates(readings, readingsStr.head.length)
  println(rates.gamma * rates.epsilon)
  val oxygenRating = extractLifeSupportRating(readings, 12, BitCriteria.oxygen)
  val co2Rating = extractLifeSupportRating(readings, 12, BitCriteria.co2)
  println(co2Rating * oxygenRating)
}

object RateExtractor {

  def extractRates(readings: List[Int], readingLength: Int): Rates = {
    val gammaRateStr = extractGammaRateStr(readings, readingLength)
    val epsilonRateStr = gammaRateStr.map(digit => if digit == "0" then "1" else "0")
    Rates(
      parseBinaryNumber(gammaRateStr.mkString),
      parseBinaryNumber(epsilonRateStr.mkString)
    )
  }

  def extractGammaRateStr(readings: List[Int], readingLength: Int): Seq[String] = {
    for {
      i <- Range(readingLength - 1, -1, -1)
      mask = Math.pow(2, i).toInt
      maskedBits = readings.map(_ & mask)
      nonZeroBits = maskedBits.count(_ != 0)
      bitStr = if nonZeroBits > (readings.size / 2) then "1" else "0"
    } yield bitStr
  }
}

def oxygenBitCriteriaFilter(mask: Int, zeroBits: Int, nonZeroBits: Int): Int => Boolean = {
  reading =>
    if (zeroBits > nonZeroBits) (reading & mask) == 0
    else (reading & mask) != 0
}

def co2BitCriteriaFilter(mask: Int, zeroBits: Int, nonZeroBits: Int): Int => Boolean = {
  reading =>
    if (zeroBits > nonZeroBits) (reading & mask) != 0
    else (reading & mask) == 0
}

enum BitCriteria {
  case oxygen, co2
}

def extractLifeSupportRating(
  readings: List[Int],
  readingLength: Int,
  bitCriteria: BitCriteria) = {

  @tailrec
  def findValue(readings: List[Int], position: Int): Int = {
    readings match {
      case head :: Nil => head
      case head :: tail =>
        val mask = Math.pow(2, position).toInt
        val maskedBits = readings.map(_ & mask)
        val nonZeroBits = maskedBits.count(_ != 0)
        val zeroBits = maskedBits.count(_ == 0)
        val filteredReadings = readings.filter(reading =>
          if (zeroBits > nonZeroBits) {
            // most readings have a zero-bit at this position
            if (bitCriteria == BitCriteria.oxygen) (reading & mask) == 0
            else (reading & mask) != 0
          }
          else {
            // most readings have a 1-bit at this position, or the amount is same
            if (bitCriteria == BitCriteria.oxygen) (reading & mask) != 0
            else (reading & mask) == 0
          })
        findValue(filteredReadings, position - 1)
      case Nil => throw Exception("readings was empty")
    }
  }

  findValue(readings, readingLength - 1)
}


def parseDiagnosticReport(input: List[String]): List[Int] = input.map(parseBinaryNumber)

def parseBinaryNumber(binaryNumber: String) = Integer.parseInt(binaryNumber, 2)

case class Rates(gamma: Int, epsilon: Int)

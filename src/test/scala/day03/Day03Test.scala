package day03

import org.scalatest.wordspec.AnyWordSpec

class Day03Test extends AnyWordSpec {

  val example = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010")

  val readings = parseDiagnosticReport(example)

  "The extraction of oxygen and co2" should {
    "extract the correct oxygen rating" in {
      val oxygenRating = extractLifeSupportRating(readings, 5, BitCriteria.oxygen)
      assert(oxygenRating == 23)
    }

    "extract the correct co2 rating" in {
      val co2Rating = extractLifeSupportRating(readings, 5, BitCriteria.co2)
      assert(co2Rating == 10)
    }
  }

  "The gamma and epsilon rate" should {

    "do the masking correctly" in {
      val parsedReading = parseBinaryNumber(example.head)
      assert(parsedReading == 4)
      assert((parsedReading & 32) == 0)
      assert((parsedReading & 16) == 0)
      assert((parsedReading & 8) == 0)
      assert((parsedReading & 4) == 4)
      assert((parsedReading & 2) == 0)
      assert((parsedReading & 1) == 0)
      assert((parsedReading & 0) == 0)
    }

    "extract correctly the gamma rate" in {
      val seq = RateExtractor.extractGammaRateStr(readings, 5)
      assert(seq.mkString == "10110")
    }

    "extract correctly both rates" in {
      val rates = RateExtractor.extractRates(readings, 5)
      assert(rates.gamma == 22)
      assert(rates.epsilon == 9)
    }
  }

  "The binary parser" should {
    "parse binary strings correctly" in {
      assert(parseBinaryNumber("1001") == 9)
      assert(parseBinaryNumber("0000") == 0)
      assert(parseBinaryNumber("0100") == 4)
      assert(parseBinaryNumber("0011") == 3)
    }
  }
}

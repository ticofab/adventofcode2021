package day01

import scala.io.Source

@main
def main(): Unit = {
  val list = Source.fromResource("day01input.sweeps").getLines().map(_.toInt).toList
  println(s"non windowed : ${extractIncreased(list)}")
  println(s"windowed     : ${extractIncreased(getWindowed(list))}")
}

def extractIncreased(measurements: List[Int]): Int = {
  val zipped = measurements zip measurements.drop(1)
  zipped.foldLeft(0){ case (acc, (a, b)) => if b > a then acc + 1 else acc }
}

def getWindowed(measurements: List[Int]): List[Int] = measurements.sliding(3).toList.map(_.sum)

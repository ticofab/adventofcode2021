package day05

import org.scalatest.wordspec.AnyWordSpec

class Day05Test extends AnyWordSpec {
  "The cloud map parser" must {
    "Evaluate the dangerous points correctly" in {
      val testCloudGrid = parseInput("day05input.clouds.test")
      println(s"parsed ${testCloudGrid.grid.size} points")
      testCloudGrid.printGrid()
      assert(testCloudGrid.dangerousPoints() == 5)
      println(s"the most dangerous point is ${testCloudGrid.mostDangerousPoint()}")
      assert(testCloudGrid.mostDangerousPoint()._2 == 2)
    }
  }
}

package day01

import org.scalatest.wordspec.AnyWordSpec


class Day01test extends AnyWordSpec {

  "The count of increased measurement" should {

    val list = List(1, 2, 2, 3, 4, 4) // windowed: (5, 7, 9, 11)
    val list2 = List(1)
    val list3 = List()
    val list4 = List(7, 88, 88, 90) // windowed: (183, 266)
    val list5 = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

    "be correct when taken singly" in {
      assert(extractIncreased(list) == 3)
      assert(extractIncreased(list2) == 0)
      assert(extractIncreased(list3) == 0)
      assert(extractIncreased(list4) == 2)
      assert(extractIncreased(list5) == 7)
    }

    "be correct when taken windowed" in {
      assert(extractIncreased(getWindowed(list)) == 3)
      assert(extractIncreased(getWindowed(list2)) == 0)
      assert(extractIncreased(getWindowed(list3)) == 0)
      assert(extractIncreased(getWindowed(list4)) == 1)
      assert(extractIncreased(getWindowed(list5)) == 5)
    }
  }

}

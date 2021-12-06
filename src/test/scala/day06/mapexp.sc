val m = Map[String, Int]("f" -> 2, "g" -> 2, "e" -> 4)
m.foldLeft(0){ case (acc, (s, n)) => acc + n }
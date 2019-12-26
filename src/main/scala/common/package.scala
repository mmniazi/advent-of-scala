package object common {
  implicit class TupleMethods(val t1: (Int, Int)) {
    def +(t2: (Int, Int)): (Int, Int) = (t1._1 + t2._1, t1._2 + t2._2)
  }
}

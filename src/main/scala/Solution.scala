import scala.annotation.tailrec

object Solution {
  def howManySqrts(A: Int, B: Int): Int = ???

  def balance(chars: List[Char]): Boolean = ???

  def solution(A: Array[Int]): Int = {
    val length = A.length

    @tailrec
    def solutionR(idx: Int): Int =
      if(idx >= length)
        -1
      else {
        val (sumLeft, sumRight) = idx match {
          case i if i == length -1 => (A.take(length - 1).foldLeft(0L)(_+_), 0)
          case i if i == 0 => (0 , A.drop(1).foldLeft(0L)(_+_))
          case _ => {
            val tmp = A.splitAt(idx)
            (tmp._1.foldLeft(0L)(_+_), tmp._2.toStream.drop(1).foldLeft(0L)(_+_))
          }
        }
        if(sumLeft == sumRight)
          idx
        else
          solutionR(idx + 1)
      }
    {
      List(1,2).zip(List(3,4)).map(e => e._1 +e._2)
    }
    solutionR(0)
  }

  type PizzaRequest = AnyRef // FIXME make case class

  def shortestPizzaWaiting(howMany: Int, orders: Iterator[PizzaRequest]): Double = ???
}

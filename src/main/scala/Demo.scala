import scala.annotation.tailrec
import scala.math._


object Demo {
  def howManySqrts(A: Int, B: Int): Int = {
    if (B < 0) 0
    else {
      val a = if (A < 0) 0 else A

      val lsqrt = sqrt(a).toInt
      val usqrt = ceil(sqrt(B)).toInt
      def inRange(el: Int) = el >= a && el <= B

      (lsqrt to usqrt).foldLeft(0)((acc, el) =>
        if (inRange(el * el)) acc + 1 else acc)
    }
  }

  def solutionShortest(A: Array[Int]): Int = {
    // fake from https://github.com/khatwaniNikhil/codility-scala/blob/master/src/com/codility/lesson1/TapeEquilibrium.scala
    val sumAccum = A.scanLeft(0L)(_ + _).tail
    (1 to A.size - 1).map(p =>
      scala.math.abs(
        (2 * sumAccum(p - 1))
          - sumAccum(A.length - 1))
    ).min.toInt
  }
  def solution(A: Array[Int]): Int = {
    val length = A.length
    val sum = A.foldLeft(0L)(_+_)
    @tailrec
    def solutionR(idx: Int, leftSum: Long): Int = {
      if(idx == length)
        -1
      else {
        val rightSum = sum - leftSum - A(idx)
        if (leftSum == rightSum)
          idx
        else {
          solutionR(idx + 1, leftSum + A(idx) )
        }
      }
    }
    if(length > 0)
      solutionR(0, 0)
    else
      -1
  }
  def solution2right(A: Array[Int]): Int = { // what is wrong with this??
  val length = A.length
    @tailrec
    def solutionR(idx: Int, leftSum: Long, rightSum: Long): Int = {
      if(leftSum == rightSum)
        idx
      else {
        if(idx + 1 == length)
          -1
        else
          solutionR(idx + 1, leftSum + A(idx), rightSum - A(idx+1))
      }
    }
    if(length > 0)
      solutionR(0, 0, A.foldLeft(0L)(_+_) - A(0))
    else
      -1
  }
  def solution2wrong(A: Array[Int]): Int = { // what is wrong with this??
    val length = A.length
    @tailrec
    def solutionR(idx: Int, leftSum: =>Long, rightSum: =>Long): Int = {
      if(idx == length)
        -1
      else if(leftSum == rightSum)
       idx
      else {
        solutionR(idx + 1, leftSum + A(idx), rightSum - A(idx+1))
      }
    }
    if(length > 0)
      solutionR(0, 0, A.foldLeft(0L)(_+_))
    else
      -1
  }

  def solutionNSquare(A: Array[Int]): Int = {
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
    solutionR(0)
  }
}


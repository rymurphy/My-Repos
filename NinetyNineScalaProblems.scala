import scala.util.Random

object NinetyNine {

  //P01
  def last[A](x:List[A]): A ={
   x match{
     case t :: Nil => t
     case _ :: lyst => last(lyst)
   }
  }
  //P02
  def penultimate[A](x: List[A]): ={
    x match{
      case s :: t :: Nil => s
      case _ :: lyst => penultimate(lyst)
    }
  //P03
  def nth[A](pos: Int, lyst: List[A]): A ={
    (pos, lyst) match{
      case (0, x :: tail) => x
      case (pos, _ :: tail) => nth(pos-1, tail)
      case (_, Nil) => throw new NoSuchElementException
    }
  }
  //P04
  def length[A](lyst: List[A]): Int ={
    lyst match{
      case Nil => 0
      case _ :: tail 1 + length(tail)
    }
  }
  //P05
  def reverse[A](lyst: List[A]): List[A] ={
    lyst match{
      case Nil => Nil
      case x :: tail => reverse(tail) ::: List(x)
    }
  }
  //P06
  def isPalindrome[A](lyst: List[A]):Boolean ={
    lyst == lyst.reverse
  }
  //P07
  def flatten(lyst: List[Any]): List[Any] = {
    lyst flatMap{
      case x: List[_] => flatten(x)
      case y => List(y)
    }
  }
  //P08
  def compress

  //P14
  def duplicate[A](lyst: List[A]): List[A] = {
    lyst flatMap{x => List(x, x)}
  }
  //P15
  def duplicateN[A](num: Int, lyst: List[A]): List[A] = {
    lyst flatMap{x => List.fill(num)(x)}
  }
  //P17
  def split[A](loc: Int, lyst: List[A]): (List[A], List[A])={
    (loc, lyst) match{
      case (0, tail) => (Nil, tail)
      case (num, head :: tail) ={
        val(lyst1, lyst2) = split(num-1, tail)
        (head :: lyst1, lyst2)
      }
      case (_, Nil) => (Nil, Nil)
    }
  }
  //P20
  def removeAt[A](num: Int, lyst: List[A]): (List[A], A)={
    lyst.splitAt(num) match{
      case (alpha, bravo :: tail) => (alpha ::: tail, bravo)
      case (_, Nil) => throw new NoSuchElementException
    }
  }
  //P21
  def insertAt[A](inpt: A, loc: Int, lyst: List[A]): List[A] = {
    lyst.splitAt(loc) match{
      case (head, tail) => head ::: input :: tail
    }
  }
  //P22
  def range(strt: Int, end: Int): List[Int] ={
    if (strt > end) Nil
    else strt :: range(strt +1, end)
  }
  //P23
  def randomSelect[A](num: Int, lyst: List[A]): List[A] ={
    num match{
      case 0 => Nil
      case _ => {
        val (remdr, elmn) = removeAt(Random.nextInt(lyst.length), lyst)
        elmn :: randomSelect(num-1, remdr)
      }
    }
  }
  //P24
  def lotto(lenOfLyst: Int, upperBound: Int): List[Int] ={
    lenOfLyst match{
      case 0 => Nil
      case _ => Random.nextInt(upperBound) :: lotto(lenOfLyst-1, upperBound)
    }
  }
  //P25
  def randomPermutate[A](lyst: List[A]): List[A] ={
    randomSelect(lyst.length, lyst)
  }
}

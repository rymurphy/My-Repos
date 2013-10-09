object NinetyNine {
  //HelperFuction from Scala Exercises for Beginners by Tony Morris
  def succ(n: Int) = n + 1
  def pred(n: Int) = n - 1
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
  //P06 -I couldn't figure out this one so I checked the answer, and they had was a builtin.
  def isPalindrome[A](lyst: List[A]):Boolean ={
    lyst == lyst.reverse
  }
  //P07
  def flatten(lyst: List[Any]): List[Any] = lyst{
    flatMap{
      case x: List[_] => flatten(x)
      case y => List(y)
    }
  }
  //P08
  def compress

}

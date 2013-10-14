

sealed trait IO[A]
case object ReadLn extends IO[String]
case class WriteLn(s: String) extends IO[Unit]
case class Const[A](a: A) extends IO[A]
case class Compose[A, B](a: IO[A], f: A => IO[B]) extends IO[B]

// An example program which reads a line and writes it out again
object ReadWrite{
  def readThenWrite: IO[Unit] =
    Compose(ReadLn, WriteLn)
}

// An implementation for IO programs which turns them into side-effects
object RunSideEffects{
  def run[A](program: IO[A]): A = program match {
    case ReadLn => readLine
    case WriteLn(s) => println(s)
    case Const(a) => a
    case Compose(a, f) => run(f(run(a)))
  }
}

// An implementation for IO programs which turns them into list transitions
object RunListTrans{
  def run[A](program: IO[A], in: List[String], out: List[String]): (A, List[String], List[String]) =
    program match {
      case ReadLn => (in.head, in.tail, out)
      case WriteLn(x)  => ((), in, x :: out)
      case Const(a) => (a, in, out)
      case Compose(ioa, f) => {
        val (a, tmpi, tmpo) = run(ioa, in, out)
        run(f(a), tmpi, tmpo)
      }
    }
}

package semanticparser

object Utils {

  /** Recursively apply f n times to initial */
  def refeed[A](n:Int, f:A => A, initial:A):A = n match {
    case 0 => initial
    case _ => refeed(n-1, f, f(initial))
  }

  def expandBitstring(set:List[List[Boolean]]):List[List[Boolean]] = set flatMap { bitstring =>
    Set[List[Boolean]](true :: bitstring, false :: bitstring)
  }

}

package semanticparser
import XPrules._

object Utils {

  /** Recursively apply f n times to initial */
  def refeed[A](n:Int, f:A => A, initial:A):A = n match {
    case 0 => initial
    case _ => refeed(n-1, f, f(initial))
  }

  def expandBitstring(set:List[List[Boolean]]):List[List[Boolean]] = set flatMap { bitstring =>
    Set[List[Boolean]](true :: bitstring, false :: bitstring)
  }

  def toBitstring(num:Int, bits:Int):IndexedSeq[Boolean] =
    ("0"*bits + num.toBinaryString).takeRight(bits).map(_ match {
      case '1' => true; case '0' => false})

  def bitstrings(bits:Int) = (0 to Math.pow(2,bits).toInt-1).map(Utils.toBitstring(_, bits))


  /** Pretty print tree */
  def prettyprint(xp:XP[Word,Word,Word,Word]):String = xp.getClass.getSimpleName +
    (xp.spec match {
      case Some(spec) => "\n├─ " + prettyprint(spec).split("\n").mkString("\n│  ") + "\n"
      case None => "\n"
    }) + (xp.head match {
      case Left(xbar) => "└─ " +  prettyprint(xbar).split("\n").mkString("\n   ")
      case Right(conj) => "└─ " + prettyprint(conj).split("\n").mkString("\n   ")
    })

  def prettyprint(xbar:Xbar[Word,Word,Word]):String = xbar.getClass.getSimpleName +
    (xbar.complement match {
      case Some(c) => "\n├─ "
      case None => (xbar.adjunct match {
        case Some(a) => "\n├─ "
        case _ => "\n└─ "
      })
    }) + (xbar.head match {
      case Left(h) => h.meta + " \033[1m" + h.asText + "\033[0m\n"
      case Right(h) => (xbar.complement, xbar.adjunct) match {
        case (None, None) => prettyprint(h).split("\n").mkString("\n   ")
        case _ => prettyprint(h).split("\n").mkString("\n|  ")
      }
    }) + (xbar.complement match {
      case Some(c) => (xbar.adjunct match {
        case Some(a) => "├─ " + prettyprint(c).split("\n").mkString("\n|  ")
        case _ => "└─ " + prettyprint(c).split("\n").mkString("\n   ")
      })
    case None => ""
    }) + (xbar.adjunct match {
      case Some(a) =>  "\n└─ " + prettyprint(a).split("\n").mkString("\n   ")
      case None => ""
    })

  def prettyprint(conj:ConjP[XP[Word,Word,Word,Word]]):String =
    "Conjunction" + "\n" + (conj.preconj match {
      case Some(p) => "├─ Preconj " + p.asText + "\n"
      case _ => ""
    }) + "├─ " + prettyprint(conj.left).split("\n").mkString("\n│  ") +
    "\n├─ \033[1m" + conj.conj.asText + "\033[0m\n└─ " +
    prettyprint(conj.right).split("\n").mkString("\n   ")

}

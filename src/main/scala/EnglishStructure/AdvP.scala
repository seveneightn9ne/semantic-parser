package semanticparser
import XPrules._

object AdvPrules {

  case class Advbar(head:Either[Adverb, Advbar],
      adjunct:Option[AdvP] = None) extends Xbar[Adverb, Word, Word] {
    val complement = None
  }
  case class AdvP(head:Either[Advbar,ConjP[AdvP]]) extends XP[Word, Adverb, Word, Word] {
    val spec = None // No Specs on AdvPs
  }
  object AdvP {
    def apply(head:Adverb) = new AdvP(Left(Advbar(Left(head))))
  }

  sealed trait Adverb extends ClosedClassWord
  object Adverb {
    case object Therefore extends Adverb
    case object Not extends Adverb
  }
}

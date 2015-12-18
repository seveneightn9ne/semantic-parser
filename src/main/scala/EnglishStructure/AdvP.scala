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
    def apply(head:AdvValues.Adv) = new AdvP(Left(Advbar(Left(Adverb(head)))))
  }


  object AdvValues extends Enumeration {
    type Adv = Value
    val Therefore, Not = Value
  }
  case class Adverb(value:AdvValues.Adv) extends ClosedClassWord[AdvValues.Adv]

}

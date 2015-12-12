package semanticparser
import XPrules._
import NPrules._

object PPrules {

  case class Pbar(head:Either[Preposition, Pbar], complement:Option[NP] = None) extends Xbar[Preposition, Noun, Word] {
    val adjunct = None
  }
  object Pbar {
    def apply(head:Preposition, complement:NP) = new Pbar(Left(head), Some(complement))
    def apply(head:Pbar) = new Pbar(Right(head))
  }
  case class PP(head:Either[Pbar,ConjP[PP]]) extends XP[Word, Preposition, Noun, Word] {
    val spec = None // No Specs on PPs
  }

  object PValues extends Enumeration {
    type P = Value
    val On, At, With, From, To, In, Of = Value
  }
  import PValues._
  case class Preposition(value:P) extends ClosedClassWord[P]

}

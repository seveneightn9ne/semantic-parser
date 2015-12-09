package semanticparser
import XPrules._
import DPrules._
import PPrules._

object NPrules {

  case class Nbar(head:Either[Noun, Nbar], adjunct:Option[PP] = None) extends Xbar[Noun, Word, Preposition] {
    val complement = None
  }
  object Nbar {
    def apply(head:Noun) = new Nbar(Left(head))
    def apply(head:Nbar, adjunct:PP) = new Nbar(Right(head), Some(adjunct))
  }
  case class NP(spec:Option[DP], head:Either[Nbar,ConjP[NP]]) extends XP[Determiner, Noun, Word, Preposition]
  object NP {
    def apply(spec:DP, head:Nbar) = new NP(Some(spec), Left(head))
    def apply(head:Nbar) = new NP(None, Left(head))
    def apply(head:Noun) = new NP(None, Left(Nbar(head)))
  }
  case class Noun(text:String) extends Word
}

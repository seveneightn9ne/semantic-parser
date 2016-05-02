package semanticparser
import XPrules._
import IPrules._
import NPrules._
import AdvPrules._

object CPrules {

  case class Cbar(head:Either[Compl, Cbar], complement:Some[IP]) extends Xbar[Compl, Infl, Adverb] {
    val adjunct = None
  }
  object Cbar {
    def apply(head:Compl, complement:IP) = new Cbar(Left(head), Some(complement))
  }
  case class CP(spec:Option[NP], head:Either[Cbar,ConjP[CP]]) extends XP[Noun, Compl, Infl, Adverb]
  object CP {
    def apply(spec:NP, head:Cbar) = new CP(Some(spec), Left(head))
    def apply(spec:NP, head:Compl, complement:IP) = new CP(Some(spec), Left(Cbar(head, complement)))
    def apply(complement:IP) = new CP(None, Left(Left(Compl("")), Some(complement)))
  }
  case class Compl(text:String) extends Word
}

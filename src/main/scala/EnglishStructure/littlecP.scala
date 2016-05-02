package semanticparser
import XPrules._
import IPrules._
import NPrules._
import CPrules._
import AdvPrules._

object cPrules {

  case class cbar(head:Either[compl, cbar], complement:Some[CP]) extends Xbar[compl, Compl, Adverb] {
    val adjunct = None
  }
  object cbar {
    def apply(head:compl, complement:CP) = new cbar(Left(head), Some(complement))
  }
  case class cP(spec:Option[NP], head:Either[cbar,ConjP[cP]]) extends XP[Noun, compl, Compl, Adverb]
  object cP {
    def apply(spec:NP, head:cbar) = new cP(Some(spec), Left(head))
    def apply(spec:NP, head:compl, complement:CP) = new cP(Some(spec), Left(cbar(head, complement)))
  }
  case class compl(text:String) extends Word
}

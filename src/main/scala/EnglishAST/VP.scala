package semanticparser
import XPrules._
import NPrules._
import PPrules._

object VPrules {

  case class Vbar(head:Either[Verb, Vbar], complement:Option[NP], adjunct:Option[PP] = None)
      extends Xbar[Verb, Noun, Preposition]
  object Vbar {
    def apply(head:Verb) = new Vbar(Left(head), None)
    def apply(head:Verb, complement:NP) = new Vbar(Left(head), Some(complement))
    def apply(head:Vbar, adjunct:PP) = new Vbar(Right(head), None, Some(adjunct))
    def apply(head:Vbar) = new Vbar(Right(head), None)
  }
  case class VP(spec:Option[NP], head:Either[Vbar,ConjP[VP]]) extends XP[Noun, Verb, Noun, Preposition]
  object VP {
    def apply(spec:NP, head:Vbar) = new VP(Some(spec), Left(head))
    def apply(head:Verb) = new VP(None, Left(Vbar(head)))
    def apply(spec:NP, head:Verb) = new VP(Some(spec), Left(Vbar(head)))
  }
  case class Verb(text:String) extends Word
}

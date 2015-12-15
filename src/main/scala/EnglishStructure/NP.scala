package semanticparser


import XPrules._
import VPrules._
import DPrules._
import PPrules._

object NPrules {

  case class Nbar(head:Either[Noun, Nbar], complement:Option[VP] = None, adjunct:Option[PP] = None)
    extends Xbar[Noun, Verb, Preposition]
  object Nbar {
    def apply(head:Noun) = new Nbar(Left(head), None)
    def apply(head:Nbar, adjunct:PP) = new Nbar(Right(head), None, Some(adjunct))
    def apply(head:Noun, compl:Option[VP]) = new Nbar(Left(head), compl)

  }
  case class NP(spec:Option[DP], head:Either[Nbar,ConjP[NP]]) extends XP[Determiner, Noun, Verb, Preposition]
  object NP {
    def apply(spec:DP, head:Noun) = new NP(Some(spec), Left(Nbar(Left(head), None)))
    def apply(head:Nbar) = new NP(None, Left(head))
    def apply(head:Noun) = new NP(None, Left(Nbar(Left(head), None, None)))
    def apply(head:ConjP[NP]) = new NP(None, Right(head))
    def apply(spec:Option[DP], head:Noun) = new NP(spec, Left(Nbar(head)))
    def apply(spec:Option[DP], head:Noun, compl:Option[VP]) = new NP(spec, Left(Nbar(head, compl)))
  }
  sealed trait Noun extends Word {
    val text:String
    val plural:Boolean
  }
  case class RegularNoun(text:String, plural:Boolean=false) extends Noun {
    override def asText = plural match {
      case true => text + "s"
      case _ => text
    }
  }
  object Noun {
    def apply(text:String, plural:Boolean=false) = try {
      Pronoun(Pron.withName(text.toLowerCase.capitalize))
    } catch {
      case e:NoSuchElementException => RegularNoun(text, plural)
    }
    def unapply(n:Noun):Option[String] = Some(n.text)
  }
  object Pron extends Enumeration {
    type Pron = Value
    val Everyone, Everything, Who, That = Value
  }
  import Pron._
  case class Pronoun(value:Pron) extends ClosedClassWord[Pron] with Noun {
    val plural = false
  }


}

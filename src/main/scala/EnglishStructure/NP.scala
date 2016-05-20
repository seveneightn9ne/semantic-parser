package semanticparser

import XPrules._
import VPrules._
import DPrules._
import PPrules._
import Features._

object NPrules {

  case class Nbar(head:Either[Noun, Nbar], complement:Option[VP] = None, adjunct:Option[PP] = None)
    extends Xbar[Noun, Verb, Preposition] {
      override def intrinsic(f:Feature) = f match {
        case _:Plural => true
        case _ => false
      }
    }
  object Nbar {
    def apply(head:Noun) = new Nbar(Left(head), None)
    def apply(head:Nbar, adjunct:PP) = new Nbar(Right(head), None, Some(adjunct))
    def apply(head:Noun, compl:Option[VP]) = new Nbar(Left(head), compl)

  }
  case class NP(head:Either[Nbar,ConjP[NP]]) extends XP[Word, Noun, Verb, Preposition] {
    val spec = None
  }
  object NP {
    def apply(head:Noun) = new NP(Left(Nbar(Left(head), None)))
    def apply(head:Nbar) = new NP(Left(head))
    def apply(head:ConjP[NP]) = new NP(Right(head))
    def apply(head:Noun, compl:Option[VP]) = new NP(Left(Nbar(head, compl)))
    def Trace(isplural:Boolean) = new NP(Left(Nbar(Noun.Trace(isplural))))
  }
  sealed trait Noun extends Word {
    val text:String
    def features = Set(plural)
    val plural:Features.Plural
    override def meta = "Noun"
  }
  case class CommonNoun(text:String, isplural:Boolean=false) extends Noun {
    val plural = Features.Plural(Some(isplural))
    override def asText = text
    /*override def asText = plural.value match {
      case Some(true) => text + "s"
      case _ => text
    }*/
    //override def meta = "CommonNoun" + (if(plural) "(plural)" else "(singular)")
  }
  case class ProperNoun(text:String) extends Noun {
    val plural = Features.Plural(Some(false))
  }
  /*case class MassNoun(text:String) extends Noun {
    val plural = Features.Plural(Some(false))
    override def meta = "MassNoun"
  }*/
  object Noun {
    def apply(text:String, plural:Boolean=false) = CommonNoun(text, plural)
    def unapply(n:Noun):Option[String] = Some(n.text)
    case class Trace(isplural:Boolean) extends Noun {
      override val plural = Features.Plural(scala.Some(isplural))
      override val meta = "trace"
      override val text = ""
      override val asText = ""
    }
  }

}

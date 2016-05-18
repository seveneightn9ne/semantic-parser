package semanticparser
import XPrules._
import DPrules._
import AdvPrules._
import Features._

object VPrules {

  case class Vbar(head:Either[Verb, Vbar], complement:Option[DP], adjunct:Option[AdvP] = None)
      extends Xbar[Verb, Determiner, Adverb]
  object Vbar {
    //def apply(head:Verb) = new Vbar(Left(head), None)
    //def apply(head:Verb, complement:DP) = new Vbar(Left(head), Some(complement))
    def apply(head:Verb, complement:Option[DP]) = new Vbar(Left(head), complement, None)
    //def apply(head:Verb, complement:Option[DP], adj:Option[AdvP]) = adj match {
    //  case Some(a) => new Vbar(Right(new Vbar(Left(head), complement, None)), None, adj)
    //  case _ => new Vbar(Left(head), complement, adj)
    //}
    //def apply(head:Vbar) = new Vbar(Right(head), None)
  }
  case class VP(spec:Option[DP], head:Either[Vbar,ConjP[VP]]) extends XP[Determiner, Verb, Determiner, Adverb] {
    override def epp(f:Feature) = f match {
      case _:Plural => true
      case _ => false
    }
  }
  object VP {
    def apply(spec:DP, head:Vbar) = new VP(Some(spec), Left(head))
    //def apply(spec:Option[DP], head:Vbar) = new VP(spec, Left(head))
    //def apply(spec:DP, head:Verb) = new VP(Some(spec), Left(Vbar(head)))
    //def apply(head:Verb, complement:DP) = new VP(None, Left(Vbar(head, complement)))
    //def apply(head:Vbar) = new VP(None, Left(head))
    //def apply(adj:Option[AdvP], spec:DP, head:Vbar) = adj match {
    //  case a:Some[AdvP] => new VP(Some(spec), Left(Vbar(Right(head), None, a)))
    //  case _ => new VP(Some(spec), Left(head))
    //}
    //def apply(adj:Option[AdvP], head:Vbar) = adj match {
    //  case a:Some[AdvP] => new VP(None, Left(Vbar(Right(head), None, adj)))
    //  case _ => new VP(None, Left(head))
    //}
  }
  case class Verb(text:String, pluralSubjAgr:Boolean=false) extends Word {
    override def asText = pluralSubjAgr match {
      case true => text
      case _ => text.takeRight(1) match {
        case "y" => text.dropRight(1) + "ies"
        case _ => text + "s"
      }
    }
    val pluralagr = Features.Plural(Some(pluralSubjAgr)) // TODO always valued?
    def features = Set(pluralagr)
    override def meta = "Verb" + (if(pluralSubjAgr) "(plural)" else "")
    override def epp(f:Feature) = f match {
      case _:Plural => true
      case _ => false
    }
  }
}

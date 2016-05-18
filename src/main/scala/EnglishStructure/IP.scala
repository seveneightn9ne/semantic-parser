package semanticparser
import XPrules._
import VPrules._
import DPrules._
import AdvPrules._
//import Features.Feature
object IPrules {

  case class Ibar(head:Either[Infl, Ibar], complement:Some[VP]) extends Xbar[Infl, Verb, Adverb] {
    val adjunct = None
  }
  object Ibar {
    def apply(head:Infl, complement:VP) = new Ibar(Left(head), Some(complement))
  }
  case class IP(spec:Option[DP], head:Either[Ibar,ConjP[IP]]) extends XP[Determiner, Infl, Verb, Adverb]
  object IP {
    def apply(spec:DP, head:Ibar) = new IP(Some(spec), Left(head))
    def apply(spec:DP, head:Infl, complement:VP) = new IP(Some(spec), Left(Ibar(head, complement)))
    def apply(spec:Option[DP], head:Ibar) = new IP(spec, Left(head))
  }
  /*object IValues extends Enumeration {
    type T = Value
    val , Can, Might = Value
  }*/
  case class Infl(text:String/*, features:Set[Feature]*/) extends Word {// TODO ClosedCLass 
    def features = Set[Features.Feature]()
    override def meta = "Infl"
    override def asText = ""
  }
  /*object Infl {
    def apply(text:String) = new Infl(text, Set(
      Features.Tense(Features.Tense.Infinitive),
      Features.Plural(None))) // This has to get a value via probe
  }*/
}

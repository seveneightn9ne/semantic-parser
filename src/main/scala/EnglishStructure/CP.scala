package semanticparser
import XPrules._
import IPrules._
import DPrules._
import AdvPrules._

object CPrules {

  case class Cbar(head:Either[Compl, Cbar], complement:Some[IP]) extends Xbar[Compl, Infl, Adverb] {
    val adjunct = None
  }
  object Cbar {
    //def apply(head:Compl, complement:IP) = new Cbar(Left(head), Some(complement))
    def apply(head:Option[Compl], complement:IP) = head match {
      case None => new Cbar(Left(Compl.Null), Some(complement))
      case Some(compl) => new Cbar(Left(compl), Some(complement))
    }
  }
  case class CP(spec:Option[DP], head:Either[Cbar,ConjP[CP]]) extends XP[Determiner, Compl, Infl, Adverb]
  object CP {
    //def apply(spec:DP, head:Cbar) = new CP(Some(spec), Left(head))
    //def apply(spec:DP, head:Compl, complement:IP) = new CP(Some(spec), Left(Cbar(head, complement)))
    def apply(complement:IP) = new CP(None, Left(Cbar(Left(Compl.Null), Some(complement))))
    def apply(spec:Option[DP], head:Cbar) = new CP(spec, Left(head))
  }
  sealed trait Compl extends ClosedClassWord {
    override def features = Set[Features.Feature]() // TODO
    override def meta = "C"
  }
  object Compl {
    case object Null extends Compl {
      override val asText = ""
    }
    case object That extends Compl {
      override val asText = "that"
    }
  }
}

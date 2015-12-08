package semanticparser
import XPrules._
import NPrules._

object DPrules {

  case class Dbar(head:Either[Determiner,Dbar]) extends Xbar[Determiner, Word, Word]{
    val complement = None
    val adjunct = None
  }
  object Dbar {
    def apply(head:Determiner) = new Dbar(Left(head))
    def apply(head:Dbar) = new Dbar(Right(head))
  }
  case class DP(spec:Option[NP], head:Dbar) extends XP[Noun, Determiner, Word, Word]
  object DP {
    def apply(spec:NP, head:Determiner) = new DP(Some(spec), Dbar(head))
    def apply(head:Determiner) = new DP(None, Dbar(head))
    def apply(head:Dbar) = new DP(None, Dbar(head))
  }

  case class Determiner extends Enumeration with ClosedClassWord {
    type Determiner = Value
    val The, A, S, Some, All, No = Value
  }
}

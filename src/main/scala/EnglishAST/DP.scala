package semanticparser
import XPrules._
import NPrules._

object DPrules {

  //import Determiner.Values._

  case class Dbar(head:Either[Determiner,Dbar]) extends Xbar[Determiner, Word, Word]{
    val complement = None
    val adjunct = None
  }
  object Dbar {
    def apply(head:Determiner) = new Dbar(Left(head))
    def apply(head:Dbar) = new Dbar(Right(head))
  }
  case class DP(spec:Option[NP], head:Either[Dbar,ConjP[DP]]) extends XP[Noun, Determiner, Word, Word]
  object DP {
    def apply(spec:NP, head:Determiner) = new DP(Some(spec), Left(Dbar(head)))
    def apply(head:Determiner) = new DP(None, Left(Dbar(head)))
    def apply(head:Dbar) = new DP(None, Left(Dbar(head)))
  }


  object DValues extends Enumeration {
    type D = Value
    val The, A, S, Some, All, No, Every = Value
    //def toDP:DP = DP(Dbar(this))
  }
  import DValues._

  case class Determiner(value:D) extends ClosedClassWord[D]
  object Determiner{
    def values = DValues.values.map(_.toString) ++ DValues.values.map(_.toString.toLowerCase)
    def isnt(thing:String):Boolean = !values.contains(thing)
  }
}

package semanticparser
import XPrules._
import IPrules._
import NPrules._
import CPrules._
import AdvPrules._

object littlecPrules {

  case class littlecbar(head:Either[littlecompl, littlecbar], complement:Some[CP])
      extends Xbar[littlecompl, Compl, Adverb] {
    val adjunct = None
  }
  object littlecbar {
    def apply(head:littlecompl, complement:CP) = new littlecbar(Left(head), Some(complement))
  }
  case class littlecP(spec:Option[NP], head:Either[littlecbar,ConjP[littlecP]])
      extends XP[Noun, littlecompl, Compl, Adverb]

  object littlecP {
    //def apply(spec:NP, head:littlecbar) = new littlecP(Some(spec), Left(head))
    //def apply(spec:NP, head:littlecompl, complement:CP) =
    //  new littlecP(Some(spec), Left(littlecbar(head, complement)))
    def apply(spec:Option[NP], compl:CP) =
      new littlecP(spec, Left(littlecbar(Left(littlecompl), Some(compl))))
  }
  sealed trait littlecompl extends ClosedClassWord {
    override val text = ""
  }
  case object littlecompl extends littlecompl {
    override val asText = ""
    override val meta = "c"
  }
}

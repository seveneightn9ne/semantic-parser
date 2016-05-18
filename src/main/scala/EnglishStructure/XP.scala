package semanticparser
import NPrules._
import VPrules._
import Features._

object XPrules {
  case class InvalidASTException(message: String) extends Exception(message)

  sealed trait hasFeatures {
    def features:Set[Feature]
    def intrinsic(feature:Feature):Boolean // Is this feature valued in the lexicon
    def epp(feature:Feature):Boolean // Does this feature have EPP
  }

  /**
   * S -> Specifier's head type
   * H -> Head type
   * C -> Complement's head type
   * A -> Adjunct's head type
   */
  trait XP[+S <: Word, +H <: Word, +C <: Word, +A <: Word] extends hasFeatures {
    val spec:Option[XP[Word, S, Word, Word]]
    val head:Either[Xbar[H, C, A],ConjP[XP[S,H,C,A]]]
    def features:Set[Feature] = head match {
      case Left(xbar) => xbar.features
      case Right(conjp) => conjp.left.features // Woah there
    }
    def epp(f:Feature) = false       // Overridden where necessary
    def intrinsic(f:Feature) = false // Overridden where necessary

    def asText: String = (head, spec) match {
      case (Left(h), Some(s)) => s.asText + " " + h.asText
      case (Right(h), Some(s)) => s.asText + " " + h.asText
      case (Left(h), None) => h.asText
      case (Right(h), None) => h.asText
    }
  }

  case class ConjP[+P <: XP[Word, Word, Word, Word]](preconj:Option[Preconj], left:P, conj:Conj, right:P) {
    def asText:String = preconj match {
      case Some(p) => p.text + " " + left.asText + " " + conj.text + " " + right.asText
      case _ => left.asText + " " + conj.text + " " + right.asText
    }
  }

  trait Xbar[+H <: Word, +C <: Word, +A <: Word] extends hasFeatures {
    val head:Either[H, Xbar[H, Word, Word]]
    val complement: Option[XP[Word, C, Word, Word]]
    val adjunct: Option[XP[Word, A, Word, Word]]
    def features:Set[Feature] = head match {
      case Left(w) => w.features
      case Right(xbar) => xbar.features
    }
    def epp(f:Feature) = false       // Overridden where necessary
    def intrinsic(f:Feature) = false // Overridden where necessary

    def asText: String = (head, complement, adjunct) match {
      case (Left(h), Some(c), None) => h.asText + " " + c.asText
      case (Right(h), Some(c), None) => h.asText + " " + c.asText
      case (Left(h), None, Some(a)) => h.asText + " " + a.asText
      case (Right(h), None, Some(a)) => h.asText + " " + a.asText
      case (Left(h), None, None) => h.asText
      case (Right(h), None, None) => h.asText
      case _ => throw new InvalidASTException(
		    "Cannot have both complement and adjunct." + super.toString)
    }

    //def isLegitimate:Boolean = head.
  }

  trait Word extends hasFeatures {
    def text:String
    def features:Set[Feature]
    def asText = text
    def meta:String = this.getClass.getName
    def epp(f:Feature) = false       // Overridden where necessary
    def intrinsic(f:Feature) = false // Overridden where necessary
    //def isLegitimate:Boolean = features.forall(_.hasValue)
  }

  /*trait ClosedClassWord[T] extends Enumeration with Word {
    val value:T
    val text = value.toString.toLowerCase
  }

  object ConjV extends Enumeration {
    type C = Value
    val And, Or = Value
  }
  import ConjV._

  object PreconjV extends Enumeration {
    type Pre = Value
    val Either, Both = Value
  }
  import PreconjV._

  case class Conj(value:C) extends ClosedClassWord[C]
  case class Preconj(value:Pre) extends ClosedClassWord[Pre]*/

  trait ClosedClassWord extends Word {
    val classname = this.getClass.getName
    def text = classname
    def features = Set[Feature]()
    //val features = Set() // TODO
  }

  sealed trait Conj extends ClosedClassWord
  sealed trait Preconj extends ClosedClassWord
  object Conj {
    case object And extends Conj
    case object Or extends Conj

    case object Either extends Preconj
    case object Both extends Preconj
  }



  type Sentence = VP


}

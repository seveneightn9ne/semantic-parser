package semanticparser
import NPrules._
import VPrules._

object XPrules {
  case class InvalidASTException(message: String) extends Exception(message)

  /**
   * S -> Specifier's head type
   * H -> Head type
   * C -> Complement's head type
   * A -> Adjunct's head type
   */
  trait XP[+S <: Word, +H <: Word, +C <: Word, +A <: Word] {
    val spec:Option[XP[Word, S, Word, Word]]
    val head:Xbar[H, C, A]

    def asText: String = spec match {
      case Some(s) => s.asText + " " + head.asText
      case _ => head.asText
    }
  }

  trait Xbar[+H <: Word, +C <: Word, +A <: Word] {
    val head:Either[H, Xbar[H, Word, Word]]
    val complement: Option[XP[Word, C, Word, Word]]
    val adjunct: Option[XP[Word, A, Word, Word]]

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
  }

  trait Word {
    val text:String
    def asText = text
  }

  trait ClosedClassWord extends Enumeration with Word {
    val text = this.toString.toLowerCase
  }

  case class Sentence(np:NP, vp:VP) {
    def asText = np.asText + " " + vp.asText + "."
  }
}

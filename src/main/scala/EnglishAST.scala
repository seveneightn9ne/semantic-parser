package semanticparser

object AST {
  case class InvalidASTException(message: String) extends Exception(message)

  /**
   * S -> Specifier's head type
   * H -> Head type
   * C -> Complement's head type
   * A -> Adjunct's head type
   */
  sealed trait XP[+S <: Word, +H <: Word, +C <: Word, +A <: Word] {
    val spec:Option[XP[Word, S, Word, Word]]
    val head:Xbar[H, C, A]

    def asText: String = spec match {
      case Some(s) => s.asText + " " + head.asText
      case _ => head.asText
    }
  }

  sealed trait Xbar[+H <: Word, +C <: Word, +A <: Word] {
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

  case class Vbar(head:Either[Verb, Vbar], complement:Option[NP], adjunct:Option[PP] = None)
      extends Xbar[Verb, Noun, Preposition]
  object Vbar {
    def apply(head:Verb) = new Vbar(Left(head), None)
    def apply(head:Verb, complement:NP) = new Vbar(Left(head), Some(complement))
    def apply(head:Vbar, adjunct:PP) = new Vbar(Right(head), None, Some(adjunct))
    def apply(head:Vbar) = new Vbar(Right(head), None)
  }
  case class VP(spec:Option[NP], head:Vbar) extends XP[Noun, Verb, Noun, Preposition]
  object VP {
    def apply(spec:NP, head:Vbar) = new VP(Some(spec), head)
    def apply(head:Verb) = new VP(None, Vbar(head))
  }

  case class Nbar(head:Either[Noun, Nbar], adjunct:Option[PP] = None) extends Xbar[Noun, Word, Preposition] {
    val complement = None
  }
  object Nbar {
    def apply(head:Noun) = new Nbar(Left(head))
    def apply(head:Nbar, adjunct:PP) = new Nbar(Right(head), Some(adjunct))
  }
  case class NP(spec:Option[DP], head:Nbar) extends XP[Determiner, Noun, Word, Preposition]
  object NP {
    def apply(spec:DP, head:Nbar) = new NP(Some(spec), head)
    def apply(head:Nbar) = new NP(None, head)
    def apply(head:Noun) = new NP(None, Nbar(head))
  }

  case class Pbar(head:Either[Preposition, Pbar], complement:Option[NP] = None) extends Xbar[Preposition, Noun, Word] {
    val adjunct = None
  }
  object Pbar {
    def apply(head:Preposition, complement:NP) = new Pbar(Left(head), Some(complement))
    def apply(head:Pbar) = new Pbar(Right(head))
  }
  case class PP(head:Pbar) extends XP[Word, Preposition, Noun, Word] {
    val spec = None // No Specs on PPs
  }

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

  sealed trait Word {
    val text:String
    def asText = text
  }

  case class Verb(text:String) extends Word
  case class Noun(text:String) extends Word

  sealed trait ClosedClassWord extends Enumeration with Word {
    val text = this.toString.toLowerCase
  }

  case class Preposition extends Enumeration with ClosedClassWord {
    type Preposition = Value
    val On, At, With, From, To, In, Of = Value
  }

  case class Determiner extends Enumeration with ClosedClassWord {
    type Determiner = Value
    val The, A, S, Some, All, No = Value
  }

  case class Sentence(np:NP, vp:VP) {
    def asText = np.asText + " " + vp.asText + "."
  }
}

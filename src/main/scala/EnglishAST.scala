package semanticparser

object AST {
  sealed trait Xbar[S,H,C,A] {
    val spec:Option[Xbar[Any,S,Any,Any]]
    val head:H
    val complement:Option[Xbar[Any,C,Any,Any]]
    val adjunct:Option[Xbar[Any,A,Any,Any]]

    override def toString = List(spec, head, complement, adjunct).flatMap{x => x match {
      case Some(i) => Some(i.toString)
      case None => None
      case i => Some(i)
    }}.mkString(" ")
  }

  case class VP(head:Verb, complement:Option[NP]) extends Xbar[Any, Verb, Noun, Any] {
    val spec = None
    val adjunct = None
  }
  case class NP(head:Noun) extends Xbar[Any, Noun, Any, Any] {
    val spec = None
    val complement = None
    val adjunct = None
  }

  sealed trait Word {
    val text:String
    override def toString = text
  }

  case class Verb(text:String) extends Word
  case class Noun(text:String) extends Word

  case class Sentence(np:NP, vp:VP) {
    override def toString = np.toString + " " + vp.toString
  }
}

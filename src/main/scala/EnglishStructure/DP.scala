package semanticparser
import XPrules._
import NPrules._
import PPrules._
import littlecPrules._

object DPrules {

  sealed trait DP extends XP[Determiner, Determiner, Word, Preposition]

  case class Dbar(head:Either[Determiner,Dbar], complement:Option[NP], adjunct:Option[PP] = None)
      extends Xbar[Determiner, Noun, Preposition]
  object Dbar {
    def apply(head:Determiner, complement:NP) = new Dbar(Left(head), Some(complement))
    def apply(head:Determiner) = new Dbar(Left(head), None)
    def apply(head:Determiner, compl:Option[NP]) = new Dbar(Left(head), compl)
  }
  case class NomDP(spec:Option[DP], head:Either[Dbar,ConjP[NomDP]]) extends DP with XP[Determiner, Determiner, Noun, Preposition]
  object DP {
    def apply(head:Determiner) = new NomDP(None, Left(Dbar(head)))
    def apply(head:Dbar) = new NomDP(None, Left(head))
    def apply(head:RelDbar) = new RelDP(None, Left(head))
    //def apply(head:Determiner, complement:NP) = new NomDP(None, Left(Dbar(head, Some(complement))))
    //def apply(head:Determiner, compl:littlecP) = new RelDP(None, Left(RelDbar(head, compl)))
  }

  case class RelDbar(head:Either[Determiner, RelDbar], complement:Option[littlecP], adjunct:Option[PP] = None)
      extends Xbar[Determiner, littlecompl, Preposition]
  object RelDbar {
    def apply(head:Determiner, compl:littlecP) = new RelDbar(Left(head), Some(compl))
  }
  case class RelDP(spec:Option[DP], head:Either[RelDbar,ConjP[RelDP]]) extends DP with XP[Determiner, Determiner, littlecompl, Preposition]

  sealed trait Determiner extends ClosedClassWord {
    def plural:Features.Plural
    override def features = Set(plural)
    override val meta = "Determiner"
  }
  object Determiner {
    sealed trait TransitiveDeterminer extends Determiner // All require NP complement
    sealed trait IntransitiveDeterminer extends Determiner

    sealed trait SingularDeterminer extends Determiner {
      def plural = Features.Plural(scala.Some(false))
    }
    sealed trait PluralDeterminer extends Determiner {
      def plural = Features.Plural(scala.Some(true))
    }
    case object A     extends TransitiveDeterminer with SingularDeterminer {
      override val asText = "a"
    }
    case object The   extends TransitiveDeterminer with SingularDeterminer {
      override val asText = "the"
    }
    case object Every extends TransitiveDeterminer with SingularDeterminer {
      override val asText = "every"
    }

    //case object S     extends TransitiveDeterminer with PluralDeterminer // Possessive
    case object Some  extends TransitiveDeterminer with PluralDeterminer {
      override val asText = "some"
    }
    case object All   extends TransitiveDeterminer with PluralDeterminer {
      override val asText = "all"
    }

    case class No(isplural:Boolean) extends TransitiveDeterminer with SingularDeterminer with PluralDeterminer {
      override val plural = Features.Plural(scala.Some(isplural))
      override val asText = "no"
    }
    case class EC(isplural:Boolean)
        extends TransitiveDeterminer with SingularDeterminer with PluralDeterminer {
      override val plural = Features.Plural(scala.Some(isplural))
      override val asText = ""
    }

    sealed trait Pronoun extends SingularDeterminer with IntransitiveDeterminer
    case object Everyone   extends Pronoun {
      override val asText = "everyone"
    }
    case object Everything extends Pronoun {
      override val asText = "everything"
    }

    sealed trait WhPronoun extends TransitiveDeterminer
    case object Who  extends WhPronoun with SingularDeterminer {
      override val asText = "who"
    }
    case class What(isplural:Boolean) extends WhPronoun with SingularDeterminer with PluralDeterminer {
      override val plural = Features.Plural(scala.Some(isplural))
      override val asText = "what"
    }

    case class There(isplural:Boolean) extends IntransitiveDeterminer
                                          with SingularDeterminer
                                          with PluralDeterminer {
      override val plural = Features.Plural(scala.Some(isplural))
      override val asText = "there"
    }
    case class Trace(isplural:Boolean) extends IntransitiveDeterminer
                                          with SingularDeterminer
                                          with PluralDeterminer {
      override val plural = Features.Plural(scala.Some(isplural))
      override val meta = "trace"
      override val asText = ""
    }

    //case class Name(text:String) extends Determiner
    //Name is a Noun whose D is? idk?

    //sealed trait Determiner(value:D) extends ClosedClassWord
    def values = "a the every some all no everyone everything who what there".split(" ")
    def isnt(thing:String):Boolean = !values.contains(thing)
  }
}

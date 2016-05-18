package semanticparser

import XPrules._
import NPrules._
import DPrules._
import VPrules._
import AdvPrules._
import Predicates._

object Translation {
  case class TranslationException(message:String) extends Exception(message)

  sealed trait Context
  object NoContext extends Context
  case class Subject(e:Entity) extends Context
  case class SubjectPredicate(e:Entity, r:BinaryRelation) extends Context

  def translate(phrase:XP[Word,Word,Word,Word], context:Context):Predicate = (phrase, context) match {
/*
    // Conclusions
    case (VP(np, Left(Vbar(Right(vbar), None, Some(AdvP(Left(Advbar(Left(Adverb(AdvValues.Therefore)), None))))))), NoContext) => np match {
      case Some(n) => Conclusion(translate(VP(n, vbar), NoContext))
      case _ => Conclusion(translate(VP(vbar), NoContext))
    }

    // Special pronoun subjects
    case (VP(Some(NP(None, Left(Nbar(Left(Pronoun(Pron.Everyone)), compl, None)))), vbar), NoContext) => {
      val subject = UniqueDesignations.variableDesignation(NoContext)
      compl match {
        case Some(vp) => Universal(subject,
          Conditional(translate(vp, Subject(subject)), translate(vbar, Subject(subject))))
        case None => Universal(subject, translate(vbar, Subject(subject)))
      }
    }
    case (VP(Some(NP(None, Left(Nbar(Left(Pronoun(Pron.Everything)), compl, None)))), vbar), NoContext)=> {
      val subject = UniqueDesignations.variableDesignation(NoContext)
      compl match {
        case Some(vp) => Universal(subject,
          Conditional(translate(vp, Subject(subject)), translate(vbar, Subject(subject))))
        case None => Universal(subject, translate(vbar, Subject(subject)))
      }
    }

    // ProperNoun subject
    case (VP(Some(NP(None, Left(Nbar(Left(ProperNoun(n)), None, None)))), vbar), NoContext) =>
      translate(vbar, Subject(UniqueDesignations.entityConstant(n)))

    // ProperNoun object
    case (NP(None, Left(Nbar(Left(ProperNoun(n)), None, None))), SubjectPredicate(e,r)) =>
      BinaryAtom(r, e, UniqueDesignations.entityConstant(n))

    // MassNoun object ("Miles eats [meat]")
    case (NP(None, Left(Nbar(Left(MassNoun(n)), None, None))), SubjectPredicate(e,r)) =>
      BinaryAtom(r, e, UniqueDesignations.entityConstant(n))

    // Non-Branching DP Every
    case (VP(Some(NP(
      Some(DP(None,Left(Dbar(Left(Determiner(DValues.Every)))))),
      Left(nbar))), vbar), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext, Conditional(translate(nbar, Subject(newContext)),translate(vbar, Subject(newContext))))
    }

    // Non-Branching DP Subject All
    case (VP(Some(NP(Some(
        DP(None, Left(Dbar(Left(Determiner(DValues.All)))))),
      Left(nbar))), vbar), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext, Conditional(translate(nbar, Subject(newContext)),translate(vbar, Subject(newContext))))
    }

    // DP ALL object
    case (NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.All)))))),
        Left(nbar)), ctx:SubjectPredicate) => {
      val newvar = UniqueDesignations.variableDesignation(ctx)
      Universal(newvar, Conditional(translate(nbar, Subject(newvar)), BinaryAtom(ctx.r, ctx.e, newvar)))
    }

    // Non-branching DP "No" -> Negative existential
    case (VP(Some(NP(Some(
        DP(None, Left(Dbar(Left(Determiner(DValues.No)))))),
        Left(nbar))), vbar), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext,
        Conditional(translate(nbar, Subject(newContext)), Negation(translate(vbar, Subject(newContext)))))
    }

    // Non-branching DP "No" with no context (came from "there are [no balloons]
    case (NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.No)))))), Left(nbar)), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext, Negation(translate(nbar, Subject(newContext))))
    }

    // NP with determiner "a", given single context, as in X is [a Y]
    case (NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.A)))))), nbar), ctx:Subject) =>
      translate(nbar, ctx)

    // NP with determiner "a" and subj-predicate context (john eats [a fish])
    case (NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.A)))))), nbar), ctx:SubjectPredicate) => {
      val newvar = UniqueDesignations.variableDesignation(ctx)
      Existential(newvar, Conjunction(translate(nbar, Subject(newvar)), BinaryAtom(ctx.r, ctx.e, newvar)))
    }

    // NP with no determiner, with single context (as in "All Marines are candycanes")
    case (NP(None, nbar), ctx:Subject) =>
      translate(nbar, ctx)

    // Forward conjunctions. Is that always right?
    case (NP(None, Right(conj)), ctx) =>
      translate(Right(conj), ctx)

    case (VP(None, Left(Vbar(Left(Verb("are",true)),
      Some(NP(Some(DP(None,Left(Dbar(Left(Determiner(DValues.No)))))), nbar)), None))), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Negation(Existential(newContext, translate(nbar, Subject(newContext))))
    }


    // Existential (no subject)
    case (VP(None, vbar), NoContext) => {
      val v = UniqueDesignations.variableDesignation(NoContext)
      Existential(v, translate(vbar, Subject(v)))
    }

    // Subclause, given a subject, forward to vbar
    case (VP(Some(NP(None, Left(Nbar(Left(Pronoun(Pron.That)), None, None)))), vbar), ctx:Subject) =>
      translate(vbar, ctx)
    case (VP(Some(NP(None, Left(Nbar(Left(Pronoun(Pron.Who)), None, None)))), vbar), ctx:Subject) =>
      translate(vbar, ctx)
*/
    case _ => throw new TranslationException("Can't translate XP phrase: \"" + phrase.asText + "\"\n" +
      Utils.prettyprint(phrase))

  }

  def translate(phrase:Either[Xbar[Word,Word,Word], ConjP[XP[Word,Word,Word,Word]]], context:Context):Predicate = (phrase, context) match {
 /*   case (Left(xbar), c) => translate(xbar, c)
    case (Right(ConjP(preconj,l:NP,Conj(ConjV.And),r:NP)), e) =>
      Conjunction(translate(l, e), translate(r, e))
    case (Right(ConjP(preconj,l:NP,Conj(ConjV.Or),r:NP)), e) =>
      Disjunction(translate(l, e), translate(r, e))*/
    case _ => throw new TranslationException("Can't translate conjunction: " + phrase)
  }

  def translate(phrase:Xbar[Word,Word,Word], context:Context):Predicate = (phrase,context) match {
/*
    // Non Branching Nbar
    case (Nbar(Left(Noun(n)), None, None), Subject(e)) =>
      Atom(UniqueDesignations.isARelation(n), e)

    // Nbar with Verbal complement
    case (Nbar(Left(Noun(n)), Some(VP(Some(NP(None,
        Left(Nbar(Left(Pronoun(_)), None, None)))), vbar)), None), Subject(e)) =>
      Conjunction(Atom(UniqueDesignations.isARelation(n), e), translate(vbar, Subject(e)))

    // is ____ (the "s" in "is" was removed for being a 1st person present suffix)
    case (Vbar(Left(Verb("i",false)), Some(np), None), e:Subject)  =>
        translate(np, e)

    // are ___ (same as above with plural subject agreement)
    case (Vbar(Left(Verb("are",true)), Some(np), None), e:Subject) =>
        translate(np, e)

    // Non branching Vbar
    case (Vbar(Left(Verb(v,p)), None, None), Subject(entity)) =>
      Atom(UniqueDesignations.doesRelation(v), entity)

    // verb with mass noun complement - make unary relation to save computation
    // Example: Brian [eats meat] => Eb. the relation is the meat-eating relation.
    case (Vbar(Left(Verb(v,pl)),
        Some(NP(None, Left(Nbar(Left(MassNoun(n)), None, None)))), None), Subject(e)) => {
      val rel = UniqueDesignations.doesRelation(v + " " + n)
      Atom(rel, e)
    }

    // Verb with complement
    case (Vbar(Left(Verb(v,p)), Some(np),None), Subject(entity)) =>
      translate(np, SubjectPredicate(entity, UniqueDesignations.binaryRelation(v)))

    // Negation
    case (Vbar(Right(vbar), None, Some(AdvP(Left(Advbar(Left(Adverb(AdvValues.Not)), None))))), ctx) =>
      Negation(translate(vbar, ctx))
*/
    case _ => throw new TranslationException(
      "Can't translate phrase: \"" + phrase.asText + "\"\n" +  Utils.prettyprint(phrase))
  }

}

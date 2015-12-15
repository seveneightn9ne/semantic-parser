package semanticparser

import XPrules._
import NPrules._
import DPrules._
import VPrules._
import Predicates._

object Translation {
  case class TranslationException(message:String) extends Exception(message)

  sealed trait Context
  object NoContext extends Context
  case class Subject(e:Entity) extends Context
  case class SubjectPredicate(e:Entity, r:BinaryRelation) extends Context

  def translate(phrase:XP[Word,Word,Word,Word], context:Context):Predicate = (phrase, context) match {

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

    // Constant NP subject
    case (VP(Some(NP(None, Left(Nbar(Left(Noun(n)), None, None)))), vbar), NoContext) =>
      translate(vbar, Subject(UniqueDesignations.entityConstant(n)))

    // Constant NP object
    case (NP(None, Left(Nbar(Left(Noun(n)), None, None))), SubjectPredicate(e,r)) =>
      BinaryAtom(r, e, UniqueDesignations.entityConstant(n))

    // Non-Branching DP Every
    case (VP(Some(NP(
      Some(DP(None,Left(Dbar(Left(Determiner(DValues.Every)))))),
      Left(nbar))), vbar), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext, Conditional(translate(nbar, Subject(newContext)),translate(vbar, Subject(newContext))))
    }

    // Non-Branching DP All
    case (VP(Some(NP(Some(
        DP(None, Left(Dbar(Left(Determiner(DValues.All)))))),
      Left(nbar))), vbar), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext, Conditional(translate(nbar, Subject(newContext)),translate(vbar, Subject(newContext))))
    }

    // Non-branching DP "No" -> Negative existential
    case (VP(Some(NP(Some(
        DP(None, Left(Dbar(Left(Determiner(DValues.No)))))),
        Left(nbar))), vbar), NoContext) => {
      val newContext = UniqueDesignations.variableDesignation(NoContext)
      Universal(newContext,
        Conditional(translate(nbar, Subject(newContext)), Negation(translate(vbar, Subject(newContext)))))
    }

    // NP with determiner "a", given single context, as in X is [a Y]
    case (NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.A)))))), nbar), ctx:Subject) =>
      translate(nbar, ctx)

    // NP with no determiner, with single context (as in "All Marines are candycanes")
    case (NP(None, nbar), ctx:Subject) =>
      translate(nbar, ctx)

    // Forward conjunctions. Is that always right?
    case (NP(None, Right(conj)), ctx) =>
      translate(Right(conj), ctx)

    // Existential (no subject)
    case (VP(None, vbar), NoContext) => {
      val v = UniqueDesignations.variableDesignation(NoContext)
      Existential(v, translate(vbar, Subject(v)))
    }

    case (VP(Some(NP(None, Left(Nbar(Left(Pronoun(Pron.That)), None, None)))), vbar), ctx:Subject) =>
      translate(vbar, ctx)

    case (VP(Some(NP(None, Left(Nbar(Left(Pronoun(Pron.Who)), None, None)))), vbar), ctx:Subject) =>
      translate(vbar, ctx)

    case _ => throw new TranslationException("Can't translate XP phrase: \"" + phrase.asText + "\" " + phrase)

  }

  def translate(phrase:Either[Xbar[Word,Word,Word], ConjP[XP[Word,Word,Word,Word]]], context:Context):Predicate = (phrase, context) match {
    case (Left(xbar), c) => translate(xbar, c)
    case (Right(ConjP(preconj,l:NP,Conj(ConjV.And),r:NP)), e) =>
      Conjunction(translate(l, e), translate(r, e))
    case (Right(ConjP(preconj,l:NP,Conj(ConjV.Or),r:NP)), e) =>
      Disjunction(translate(l, e), translate(r, e))
    case _ => throw new TranslationException("Can't translate conjunction: " + phrase)
  }

  def translate(phrase:Xbar[Word,Word,Word], context:Context):Predicate = (phrase,context) match {

    // Non Branching Nbar
    case (Nbar(Left(Noun(n)), None, None), Subject(e)) =>
      Atom(UniqueDesignations.isARelation(n), e)

    // is ____ (the "s" in is was removed for being a 1st person present suffix)
    case (Vbar(Left(Verb("i",false)), Some(np), None), e:Subject)  =>
        translate(np, e)

    // are ___ (same as above with plural subject agreement)
    case (Vbar(Left(Verb("are",true)), Some(np), None), e:Subject) =>
        translate(np, e)

    // Non branching Vbar
    case (Vbar(Left(Verb(v,p)), None, None), Subject(entity)) =>
      Atom(UniqueDesignations.doesRelation(v), entity)

    // Verb with complement
    case (Vbar(Left(Verb(v,p)), Some(np),None), Subject(entity)) =>
      translate(np, SubjectPredicate(entity, UniqueDesignations.binaryRelation(v)))

    case _ => throw new TranslationException(
      "Can't translate phrase: \"" + phrase.asText + "\" " +  phrase)
  }

}

package semanticparser

import XPrules._
import NPrules._
import DPrules._
import VPrules._
import PredicateCalculus._

object Translation {
  def translate(phrase:XP[Word,Word,Word,Word], context:Option[Entity]):Predicate = (phrase, context) match {

    // Special NP subjects (because we haven't done Pronouns in X')
    case (VP(Some(NP(None, Left(Nbar(Left(Noun("Everyone",false)), None)))), vbar), None) => {
      val newContext = UniqueDesignations.variableDesignation
      Universal(newContext, translate(vbar, Some(newContext)))
    }

    // Constant NP subject
    case (VP(Some(NP(None, Left(Nbar(Left(Noun(n, false)), None)))), vbar), None) =>
      translate(vbar, Some(UniqueDesignations.entityConstant(n)))

    // Non-Branching DP Every
    case (VP(Some(NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.Every)))))), Left(nbar))), vbar), None) => {
      val newContext = UniqueDesignations.variableDesignation
      Universal(newContext, Conditional(translate(nbar, Some(newContext)),translate(vbar, Some(newContext))))
    }

    // Non-Branching DP All
    case (VP(Some(NP(Some(
        DP(None, Left(Dbar(Left(Determiner(DValues.All)))))),
      Left(nbar))), vbar), None) => {
      val newContext = UniqueDesignations.variableDesignation
      Universal(newContext, Conditional(translate(nbar, Some(newContext)),translate(vbar, Some(newContext))))
    }

    // Non-branching DP "No" -> Negative existential
    case (VP(Some(NP(Some(
        DP(None, Left(Dbar(Left(Determiner(DValues.No)))))),
        Left(nbar))), vbar), None) => {
      val newContext = UniqueDesignations.variableDesignation
      Universal(newContext,
        Conditional(translate(nbar, Some(newContext)), Negation(translate(vbar, Some(newContext)))))
    }

    // NP with determiner "a", given context, as in X is [a Y]
    case (NP(Some(DP(None, Left(Dbar(Left(Determiner(DValues.A)))))), nbar), Some(e)) =>
      translate(nbar, Some(e))

    // NP with no determiner, with context (as in "All Marines are candycanes")
    case (NP(None, nbar), Some(e)) =>
      translate(nbar, Some(e))

    // Forward conjunctions. Is that always right?
    case (NP(None, Right(conj)), e) =>
      translate(Right(conj), e)

    case _ => throw new RuntimeException("Can't translate XP phrase: " + phrase)

  }

  def translate(phrase:Either[Xbar[Word,Word,Word], ConjP[XP[Word,Word,Word,Word]]], context:Option[Entity]):Predicate = (phrase, context) match {
    case (Left(xbar), c) => translate(xbar, c)
    case (Right(ConjP(preconj,l:NP,Conj(ConjV.And),r:NP)), e) =>
      Conjunction(translate(l, e), translate(r, e))
    case (Right(ConjP(preconj,l:NP,Conj(ConjV.Or),r:NP)), e) =>
      Disjunction(translate(l, e), translate(r, e))
    case _ => throw new RuntimeException("Can't translate conjunction: " + phrase)
  }

  def translate(phrase:Xbar[Word,Word,Word], context:Option[Entity]):Predicate = (phrase,context) match {

    // Non Branching Nbar
    case (Nbar(Left(Noun(n,p)), None), Some(e)) =>
      Atom(UniqueDesignations.isARelation(n), e)

    // is ____ (the "s" in is was removed for being a 1st person present suffix)
    case (Vbar(Left(Verb("i",false)), Some(np), None), e)  =>
        translate(np, e)

    // are ___ (same as above with plural subject agreement)
    case (Vbar(Left(Verb("are",true)), Some(np), None), e) =>
        translate(np, e)

    // Non branching Vbar
    case (Vbar(Left(Verb(v,p)), None, None), Some(entity)) =>
      Atom(UniqueDesignations.doesRelation(v), entity)

    case _ => throw new RuntimeException("Can't translate Xbar phrase: " + phrase)
  }

}

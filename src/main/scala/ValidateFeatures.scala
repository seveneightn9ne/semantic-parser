package semanticparser
import XPrules._
import DPrules.Determiner.TransitiveDeterminer
import DPrules.Determiner.IntransitiveDeterminer
import DPrules._
import Features.Feature

object ValidateFeatures {
  case class AgreementException(message:String) extends Exception(message)

  def valid(phrases:Stream[XP[Word,Word,Word,Word]]) = phrases filter validateTransitivity filter validateFeatures

  def validateTransitivity(phrase:XP[Word,Word,Word,Word]):Boolean = phrase.spec match {
    case Some(spec) => validateTransitivity(spec) && validateTransitivity(phrase.head)
    case _ => validateTransitivity(phrase.head)
  }
  def validateTransitivity(phrase:Either[Xbar[Word,Word,Word],ConjP[XP[Word,Word,Word,Word]]]):Boolean = phrase match {
    case Left(xbar) => validateTransitivity(xbar)
    case Right(conjp) => validateTransitivity(conjp.left) && validateTransitivity(conjp.right)
  }
  def validateTransitivity(phrase:Xbar[Word,Word,Word]):Boolean = (phrase.adjunct match{
    case Some(xp) => validateTransitivity(xp)
    case None => true
  }) && (phrase.head match {
    case Right(xbar) => validateTransitivity(xbar)
    case Left(word) => word match {
      case _:TransitiveDeterminer => phrase.complement match {
        case Some(xp) => validateTransitivity(xp)
        case None => false
      }
      case _:IntransitiveDeterminer => phrase.complement match {
        case Some(_) => false
        case None => true
      }
      case _ => true
    }
  })

  def validateFeatures(xp:XP[Word,Word,Word,Word]):Boolean = xp.spec match {
    case Some(yp) => {
      println(Utils.inline(xp) + " has to agree with " + Utils.inline(yp))
      agree(xp, yp, true) && validateFeatures(yp) && validateFeatures(xp.head)
    }
    case _ => validateFeatures(xp.head)
  }
  def validateFeatures(phrase:Either[Xbar[Word,Word,Word],ConjP[XP[Word,Word,Word,Word]]]):Boolean = phrase match {
    case Left(xbar) => validateFeatures(xbar)
    case Right(conjp) => validateFeatures(conjp.left) && validateFeatures(conjp.right)
  }
  def validateFeatures(xbar:Xbar[Word,Word,Word]):Boolean = xbar.head match {
    case Right(xbar2) => xbar.adjunct match {
      case Some(yp) => validateFeatures(yp) && validateFeatures(xbar2)
      case None => validateFeatures(xbar2)
    }
    case Left(word) => xbar.complement match {
      case Some(yp) => {
        println(word + " has to agree with " + Utils.inline(yp))
        println(word.features)
        println(word.features.filter(f => word.epp(f) && !word.intrinsic(f)))
        agree(word, yp, false) && validateFeatures(yp) //TODO combine with transitivity?
      }
      case _ => true
    }
  }

  def agree(xp:hasFeatures, yp:XP[Word,Word,Word,Word], epp:Boolean) = xp.features.filter{ f => {
    println("HEY " + f + xp.epp(f) + xp.intrinsic(f))
    xp.epp(f) == epp && !xp.intrinsic(f) // All features on XP which need to have agreed with YP
    }}.forall{ f => {
    println("Feature " + f + " on " + xp + " has to agree with the probe result")
    val p = probe(yp, f)
    println("Probe result: " + p)
    p._1.exists{ _.value == f.value }
  }
  }

  // TODO phase boundary between CP & TP prevents probe?
  def probe(xp:XP[Word,Word,Word,Word], feature:Feature):(Set[Feature], Int, Set[Word]) = (xp.spec, xp.head) match{
    case (Some(spec), Left(xbar)) => {
      val(f1, d1, w1) = probe(spec, feature)
      val(f2, d2, w2) = probe(xbar, feature)
      if (f1.size == 0) return (f2, d2+1, w2)
      if (f2.size == 0) return (f1, d1+1, w1)
      if (d1 < d2) return (f1, d1+1, w1)
      if (d2 < d1) return (f2, d2+1, w2)
      if (d1 == d2) return (f1 ++ f2, d1+1, w1 ++ w2)
      throw AgreementException("Impossible state: " + f1 + f2 + d1 + d2)
    }
    case (None, Left(xbar)) => {
      val(f1, d1, w1) = probe(xbar, feature)
      return (f1, d1+1, w1)
    }
    case _ => throw AgreementException("Don't know how to probe ConjP")
  }

  // TODO ignoring adjuncts for now
  def probe(xbar:Xbar[Word,Word,Word], feature:Feature):(Set[Feature], Int, Set[Word]) = xbar.head match {
    case Left(head) => {
      val f = head.features.filter{_.getClass == feature.getClass}
      //println("Probe found " + head)
      if (f.size > 0) (f, 0, Set(head))
      else xbar.complement match {
        case Some(yp) => {
          val(f1, d1, w1) = probe(yp, feature)
          (f1, d1+1, w1)
        }
        case _ => (Set(), 0, Set())
      }
    }
    case Right(xbar) => {
      val(f1, d1, w1) = probe(xbar, feature)
      (f1, d1+1, w1)
    }
  }



}

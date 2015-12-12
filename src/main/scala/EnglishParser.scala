package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import XPrules._
import NPrules._
import VPrules._
import DPrules._

object EnglishParser extends SentenceParser with RegexParsers {

  lazy val sentences = (sentence+) ^^ {x=>x}
  lazy val sentence = vp <~ "." <~ ("\n".r).? ^^ {vp => vp}

  lazy val singularnp:Parser[NP] = (noun  ^^ { n => NP(n) }
      | singulardp ~ "#" ~ noun ^^ { (dp,_, n) => NP(dp, n) }
      | (preconj <~ "#").? ~ singularnp ~ "#" ~ conj ~ "#" ~ singularnp ^^ {(p,l,_,c,_,r) =>
          NP(ConjP[NP](p, l, c, r))}
  )
  lazy val singulardp = singulardet ^^ {d => DP(d)}
  lazy val vp = (singularnp ~ "#" ~ singularverb ^^ {(np,_,v) => VP(np, v)}
    | singularnp ~ "#" ~ singularverb ~ "#" ~ singularnp ^^ {(subj,_,v,_,obj) => VP(subj,Vbar(v, obj))})

  // Words
  lazy val singulardet = ("[Ee]very".r ^^ {d => Determiner(DValues.Every)}
    | "a".r ^^ {d => Determiner(DValues.A)})
  lazy val preconj = ("[Ee]ither]".r ^^ {p => Preconj(PreconjV.Either)}
    | "[Bb]oth".r ^^ {p => Preconj(PreconjV.Both)})
  lazy val conj = ("and".r ^^ {c => Conj(ConjV.And)}
    | "or".r ^^ {c => Conj(ConjV.Or)})
  lazy val singularverb = "\\w+s".r ^^ Verb
  lazy val noun = "\\w+".r ^^ Noun


  def parser = sentences

}

package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import XPrules._
import NPrules._
import VPrules._
import DPrules._

object EnglishParser extends SentenceParser with RegexParsers {

  lazy val sentence = vp <~ "." <~ ("\n".r).? ^^ {vp => vp}
  lazy val singularnp = (noun  ^^ { n => NP(n) }
      | singulardp ~ "#" ~ noun ^^ { (dp,_, n) => NP(dp, n) }
  )
  lazy val noun = "\\w+".r ^^ Noun
  lazy val singulardp = singulardet ^^ {d => DP(d)}
  lazy val singulardet = ("[Ee]very".r ^^ {d => Determiner(DValues.Every)}
    | "a".r ^^ {d => Determiner(DValues.A)})
  lazy val vp = (singularnp ~ "#" ~ singularverb ^^ {(np,_,v) => VP(np, v)}
    | singularnp ~ "#" ~ singularverb ~ "#" ~ singularnp ^^ {(subj,_,v,_,obj) => VP(subj,Vbar(v, obj))})
  lazy val singularverb = "\\w+s".r ^^ Verb

  lazy val sentences = (sentence+) ^^ {x=>x}

  def parser = sentences

}

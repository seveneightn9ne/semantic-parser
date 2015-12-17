package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import XPrules._
import NPrules._
import VPrules._
import DPrules._
import AdvPrules._

object EnglishParser extends SentenceParser with RegexParsers {
  val bannedWords = Determiner.values + "There"

  lazy val sentences = sentence+
  lazy val sentence = "\\s*".r ~> vp <~ "." <~ "\\s*".r ^^ {vp => vp}

  ////////////////////////// Noun Phrases
  lazy val np:Parser[NP] = (singularnp | pluralnp)
  lazy val pluralwhnp:Parser[NP] = pluralwhnoun ^^ {NP(_)}
  lazy val singularwhnp:Parser[NP] = singularwhnoun ^^ {NP(_)}
  lazy val pluralnp:Parser[NP] = (pluraldp <~ " ").? ~ pluralnoun ~ (" " ~> pluralcomplvp).? ^^ {(dp, n, vp) => NP(dp, n, vp)}
  lazy val singularnp:Parser[NP] = (
      (singulardp <~ " ") ~ noun ~ (" " ~> singularcomplvp).? ^^ {(dp, n, vp) => NP(Some(dp), n, vp)}
    | propernoun ^^ {NP(_)}
    | massnoun   ^^ {NP(_)}
    | (preconj <~ " ").? ~ (singularnp <~ " ") ~ (conj <~ " ") ~ singularnp ^^ {(p,l,c,r) =>
          NP(ConjP[NP](p, l, c, r))}
  )

  ////////////////////////// Determiner Phrases
  lazy val singulardp = singulardet ^^ {d => DP(d)}
  lazy val pluraldp = pluraldet ^^ {d => DP(d)}

  ////////////////////////// Verb Phrases (sentences :)
  lazy val vp = (
      therefore.? ~ ("[Tt]here ".r ~> vbar)             ^^ {(t,v) => VP(t,v)}
    | therefore.? ~ (singularnp <~ " ") ~ singularvbar  ^^ {(t,np,v) => VP(t,np, v)}
    | therefore.? ~ (pluralnp <~ " ") ~ pluralvbar      ^^ {(t,np,v) => VP(t,np, v)}
  )
  lazy val pluralcomplvp = (pluralwhnp <~ " ") ~ pluralvbar ^^ {(n,v) => VP(n,v)}
  lazy val singularcomplvp = (singularwhnp <~ " ") ~ singularvbar ^^ {(n,v) => VP(n,v)}
  lazy val vbar = pluralvbar | singularvbar
  lazy val pluralvbar = pluralverb ~ (" " ~> np).? ^^ {(v, onp) => Vbar(v, onp)}
  lazy val singularvbar = singularverb ~ (" " ~> np).? ^^ {(v, onp) => Vbar(v, onp)}

  ////////////////////////// Words
  lazy val singulardet = (
      "[Ee]very".r  ^^ {d => Determiner(DValues.Every)}
    | "a".r         ^^ {d => Determiner(DValues.A)}
    | "[Nn]o".r     ^^ {d => Determiner(DValues.No)})
  lazy val pluraldet = (
      "[Aa]ll".r    ^^ {d => Determiner(DValues.All)}
    | "[Ss]ome".r   ^^ {d => Determiner(DValues.Some)}
    | "[Nn]o".r     ^^ {d => Determiner(DValues.No)})

  lazy val preconj = (
      "[Ee]ither".r ^^ {p => Preconj(PreconjV.Either)}
    | "[Bb]oth".r   ^^ {p => Preconj(PreconjV.Both)})
  lazy val conj = (
      "and".r       ^^ {c => Conj(ConjV.And)}
    | "or".r        ^^ {c => Conj(ConjV.Or)})

  lazy val singularverb = "\\w+s".r ^^ {v => Verb(v.dropRight(1))} filter(goodWord)
  lazy val pluralverb   = "\\w+".r  ^^ {v => Verb(v, true)} filter(v =>
      !v.asText.endsWith("s") && goodWord(v))
  lazy val noun         = "\\w+".r  ^^ {n => Noun(n,false)} filter(goodWord)
  lazy val propernoun   = "[A-Z]\\w+".r ^^ {ProperNoun(_)} filter(goodWord)
  lazy val massnoun     = "\\w+".r ^^ {MassNoun(_)} filter(goodWord)
  lazy val pluralnoun   = "\\w+s".r ^^ {n => Noun(n.dropRight(1), true)}
  lazy val pluralwhnoun = (
      "[Ww]ho".r ^^ {n => Noun(n, true)}
    | "[Tt]hat".r ^^ {n => Noun(n, true)})
  lazy val singularwhnoun = (
      "[Ww]ho".r ^^ {n => Noun(n, false)}
    | "[Tt]hat".r ^^ {n => Noun(n, false)})
  lazy val therefore = "[Tt]herefore".r  <~ ",".? <~ " " ^^ {a => AdvP(AdvValues.Therefore)}
  def goodWord(w:Word) = !bannedWords.contains(w.asText)

  def parser = sentences
  override val whitespace = "".r

}

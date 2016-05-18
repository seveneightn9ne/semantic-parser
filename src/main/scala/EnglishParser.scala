package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import XPrules._
import NPrules._
import VPrules._
import DPrules._
import AdvPrules._
import IPrules._
import CPrules._
import littlecPrules._

object EnglishParser extends SentenceParser with RegexParsers {
  val bannedWords = Determiner.values ++ List("there", "is", "are")

  lazy val sentences = sentence+
  lazy val sentence = "\\s*".r ~> barecp <~ "." <~ "\\s*".r

  ////////////////////////// Noun Phrases
  lazy val np:Parser[NP] = nbar ^^ {NP(_)}
  lazy val nbar:Parser[Nbar] = noun ^^ {Nbar(_)}
  lazy val noun:Parser[Noun] = /*massNoun |*/ pluralNoun | singularNoun | properNoun
  lazy val pluralNoun = "\\w+s".r ^^ {CommonNoun(_, true)} filter(goodWord)
  lazy val singularNoun = "\\w+([a-r]|[t-z])".r ^^ {CommonNoun(_)} filter goodWord
  lazy val properNoun = "[A-Z]\\w+".r ^^ {ProperNoun(_)} filter goodWord

  ////////////////////////// Determiner Phrases
  lazy val dp:Parser[DP] = ((transitivedbar | intransitivedbar | ecdbar) ^^ {dbar => DP(dbar)}
    | reldbar ^^ {dbar => DP(dbar)})
  lazy val transitivedbar = ((transitivedet <~ " ") ~ np ^^ {(d, np) => Dbar(d, np)} //| possesive
    | transitivedet ^^ {Dbar(_, NP.Trace(true))}
    | transitivedet ^^ {Dbar(_, NP.Trace(false))})
  lazy val intransitivedbar = (everyone | everything) ^^ {Dbar(_)}
  lazy val ecdbar =
    ( np ^^ {np => Dbar(Determiner.EC(true), np)}
    | np ^^ {Dbar(Determiner.EC(false), _)})
  lazy val reldbar = (transitivedet <~ " ") ~ littlecp ^^ {(d,c) => RelDbar(d,c)}
  lazy val everyone = "[Ee]veryone".r ^^ {e => Determiner.Everyone}
  lazy val everything = "[Ee]verything".r ^^ {e => Determiner.Everything}
  lazy val transitivedet = a | the | every | some | all | no | whpronoun
  lazy val a     = "[Aa]".r     ^^ {a => Determiner.A}
  lazy val the   = "[Tt]he".r   ^^ {a => Determiner.The}
  lazy val every = "[Ee]very".r ^^ {a => Determiner.Every}
  lazy val some  = "[Ss]ome".r  ^^ {a => Determiner.Some}
  lazy val all   = "[Aa]ll".r   ^^ {a => Determiner.All}
  lazy val no    = "[Nn]o".r    ^^ {a => Determiner.No(true)} |
                   "[Nn]o".r    ^^ {a => Determiner.No(false)}
  lazy val there = "[Tt]here".r ^^ {a => Determiner.There(true)} |
                   "[Tt]here".r ^^ {a => Determiner.There(false)}
  lazy val whpronoun = (
      "[Ww]ho".r ^^ {who => Determiner.Who}
    | "[Ww]hat".r ^^ {what => Determiner.What(true)}
    | "[Ww]hat".r ^^ {what => Determiner.What(false)})


  ////////////////////////// IP, CP, cP
  lazy val barecp = ip ^^ {ip => CP(ip)}
  lazy val ip = (dp <~ " ").? ~ ibar ^^ {(subj, ibar) => IP(subj, ibar)}
  lazy val ibar = vp ^^ {vp => Ibar(Infl(""), vp)}
  lazy val littlecp = (np <~ " ") ~ cp ^^ {(np, cp) => littlecP(Some(np), cp)}
  lazy val cp = (dp <~ " ").? ~ cbar ^^ {(dp, cbar) => CP(dp, cbar)}
  lazy val cbar = compl.? ~ ip ^^ {(compl, ip) => Cbar(compl, ip)}
  lazy val compl = "that".r ^^ {_ => Compl.That}

  ////////////////////////// Verb Phrases (sentences :)
  lazy val vp = (vbar ^^ {vbar => VP(DP(Determiner.Trace(true)), vbar)}
    | vbar ^^ {vbar => VP(DP(Determiner.Trace(false)), vbar)})
  lazy val vbar = verb ~ (" " ~> dp).? ^^ {(v,d) => Vbar(v,d)}
  lazy val verb = singularverb | pluralverb
  lazy val singularverb = "\\w+s".r ^^ {v => Verb(v.dropRight(1))} filter(goodWord)
  lazy val pluralverb   = "\\w+".r  ^^ {v => Verb(v, true)} filter(v =>
      !v.asText.endsWith("s") && goodWord(v))
  // TODO Negation with NegP

  ////////////////////////// Words
  lazy val preconj = (
      "[Ee]ither".r ^^ {p => Conj.Either}
    | "[Bb]oth".r   ^^ {p => Conj.Both})
  lazy val conj = (
      "and".r       ^^ {c => Conj.And}
    | "or".r        ^^ {c => Conj.Or})

  //lazy val therefore = "[Tt]herefore".r  <~ ",".? <~ " " ^^ {a => Adverb.Therefore}
  //lazy val pluralneg = "don't" ^^ {a => Adverb.Not}
  //lazy val singularneg = "doesn't" ^^ {a => Adverb.Not}
  //lazy val negpart = ("n't" | " not") ^^ {a => Adverb.Not}
  def goodWord(w:Word) = !bannedWords.contains(w.asText.toLowerCase)

  def parser = dp
  override val whitespace = "".r

}

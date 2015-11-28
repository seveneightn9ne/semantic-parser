package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import AST._

object EnglishParser extends SentenceParser with RegexParsers {

  lazy val sentence = np ~ " " ~ vp <~ "." ^^ {(np,_, vp) => Sentence(np, vp)}
  lazy val np = noun  ^^ {noun => NP(noun)}
  lazy val noun = """\w+""".r ^^ {w => Noun(w)}
  lazy val vp = ( verb          ^^ {v => VP(v, None)}
                | verb ~ " " ~ np ^^ {(v,_,np) => VP(v, Some(np))}
  )
  lazy val verb = """\w+""".r ^^ {w => Verb(w)}

  def parser = sentence

}

package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import AST._

object EnglishParser extends SentenceParser with RegexParsers {

  lazy val sentence = np ~ " " ~ vp <~ "." ^^ {(np,_, vp) => Sentence(np, vp)}
  lazy val np = noun  ^^ { n => NP(n) }
  lazy val noun = "\\w+".r ^^ Noun
  //lazy val vp = ( verb          ^^ {v => VP(v, None)}
  //              | verb ~ " " ~ np ^^ {(v,_,np) => VP(v, Some(np))}
  //)
  //lazy val vp = verb ~ " " ~ np ^^ {(v,_,np) => VP(v, Some(np))}
  lazy val vp = verb ^^ {v => VP(v)}
  lazy val verb = "\\w+".r ^^ Verb

  def parser = sentence

}

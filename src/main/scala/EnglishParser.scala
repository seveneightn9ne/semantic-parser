package semanticparser

import scala.io.Source
import scala.collection.mutable

import com.codecommit.gll._
import XPrules._
import NPrules._
import VPrules._

object EnglishParser extends SentenceParser with RegexParsers {

  lazy val sentence = vp <~ "." ^^ {vp => vp}
  lazy val np = noun  ^^ { n => NP(n) }
  lazy val noun = "\\w+".r ^^ Noun
  lazy val vp = np ~ " " ~ verb ^^ {(np,_,v) => VP(np, v)}
  lazy val verb = "\\w+".r ^^ Verb

  def parser = sentence

}

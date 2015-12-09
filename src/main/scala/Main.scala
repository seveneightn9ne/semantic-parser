package semanticparser

import XPrules._
import NPrules._
import VPrules._

object Main {
  
  def main(args: Array[String]) {
    /*println(
      VP(
        NP(Nbar(Noun("Jackie"))),
        Vbar(Verb("walks"))))*/
   val s = VP(NP(Noun("Jackie")), Verb("walks"))
   println(s)
   println(PredicateCalculus.translate(s))
  }

}

package semanticparser

import AST._
object Main {
  
  def main(args: Array[String]) {
    /*println(
      VP(
        NP(Nbar(Noun("Jackie"))),
        Vbar(Verb("walks"))))*/
   val s = Sentence(NP(Noun("Jackie")), VP(Verb("walks")))
   println(s)
   println(PredicateCalculus.translate(s))
  }

}

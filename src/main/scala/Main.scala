package semanticparser

import AST._
object Main {
  
  def main(args: Array[String]) {
    println(
      VP(
        NP(Nbar(Noun("Jackie"))),
        Vbar(Verb("walks"))))
  }

}

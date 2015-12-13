package semanticparser

import scala.collection.mutable
import scala.io.Source
import com.codecommit.gll._
import XPrules._

trait SentenceParser extends Parsers {
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("Output:")
      val results = parser(LineStream(Source fromFile file))
      if (results exists {_.isInstanceOf[Success[List[Sentence]]]}) {
	handleSuccesses(for (Success(tree, _) <- results) yield tree)
      } else {
	val sorted = results.toList sortWith {_.tail.length < _.tail.length}
	val length = sorted.head.tail.length

	for (Failure(msg, tail) <- sorted takeWhile {_.tail.length == length}) {
	  tail.printError("  error:%%d: %s%n    %%s%n    %%s%n".format(msg))(System.err)
	}
      }
      println()
    }
  }

  def parser: Parser[List[Sentence]]

  def handleSuccesses(forest: Stream[List[Sentence]]) {
    val errors = mutable.Set[String]()

    val status = for (sentences <- forest) yield {
      Some(sentences)
    }

    val results = status flatMap { x => x }

    if (results.length == 0) {
      for (msg <- errors) {
	println("  runtime error: " + msg)
      }
    } else if (results.length == 1) {
      //println("  " + results.head.toString)
      println("  " + results.head.map(s => s.asText).mkString(". "))
      //println("  " + results.head.mkString("\n"))
      val predicates = results.head.map(s => Translation.translate(s, None))
      println("  " + predicates.mkString("\n  "))
      val conclusions = Conclusions.generateConclusions(predicates.toSet)
      if (conclusions.size > 0) {
        println("  Therefore:\n  " + conclusions.map{p => p.toString + " " + p.toEnglish}.mkString("\n  "))
      } else {
        println("  I don't have any conclusions to draw.")
      }
    } else {
      printf("  parse error: Ambiguous parse: %s valid trees%n", results.length.toString)
      results.foreach(l => println(l))
    }
  }
}

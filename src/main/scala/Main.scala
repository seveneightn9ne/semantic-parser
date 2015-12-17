package semanticparser

import scala.collection.mutable
import scala.io.Source
import com.codecommit.gll._
import XPrules._
import Translation._
import Predicates._

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
      println("  " + results.head.map(s => s.asText).mkString(". "))
      try {
        val predicates = results.head.map(s => translate(s, NoContext))
        concludeOrValidate(predicates)
      } catch {
        case e:TranslationException =>
          println("  Translation failure: " + e.message)
      }
    } else {
      printf("  Warning: Ambiguous parse: %s valid trees%n", results.length.toString)
      val goodresults:List[List[Predicate]] =
        results.flatMap{list => allExists[Predicate](list.map{sentence => try {
        Some(translate(sentence, NoContext))
      } catch {
        case e:TranslationException => None
      }})}.toList
      if (goodresults.length == 1) {
        println("  But there was only one surviving translation! :)\n")
        concludeOrValidate(goodresults.toList.head)
      } else {
        println("  Error: No parses survived translation. Here were the parses:")
        results.foreach{l => l.foreach{s => {
          try {
            translate(s,NoContext)
          } catch {
            case e:TranslationException => {
              println(Utils.prettyprint(s))
              println(e.message)
            }
          }
        }}}
      }
    }
  }

  def allExists[A](l:List[Option[A]]):Option[List[A]] = {
    val flat = l.flatMap{x=>x}
    flat.length == l.length match {
      case true => Some(flat)
      case _ => None
    }
  }

  def concludeOrValidate(predicates:List[Predicate]) =
    if(predicates.exists(_ match {case c:Conclusion => true; case _ => false}))
      validate(predicates.filter(_ match {case c:Conclusion => false; case _=> true}),
        predicates.flatMap(_ match {case c:Conclusion => Some(c); case _ => None}))
    else conclude(predicates)

  def conclude(predicates:List[Predicate]) = {
    println("  " + predicates.mkString("\n  "))
    println("\n  Generating conclusions...")
    val numU = Conclusions.predictNumUniverses(predicates.toSet)
    if (numU > Math.pow(2, 22)) {
      println("  Too many possible universes to evaluate all of them. :(")
    } else {
      //println("  Evaluating " + numU + " possible universes. This will take a while...")
      val conclusions = Conclusions.generateInterestingConclusions(predicates.toSet)
      if (conclusions.size > 0) {
        println("  Therefore:\n  " +
          conclusions.map{p => p.toString + " \"" + p.toEnglish.capitalize + "\""}.mkString("\n  "))
      } else {
        println("  I don't have any interesting conclusions to draw.")
      }
    }
  }

  def validate(priors:List[Predicate], conclusions:List[Conclusion]) = {
    println("  " + (priors++conclusions).mkString("\n  "))
    println("\n  Validating your conclusion...")
    val universes = Conclusions.possibleUniverses(priors.toSet)
    if(conclusions.forall(c => universes.forall(u => c.evaluate(u))))
      println("  Valid conclusion!")
    else
      println("  Invalid conclusion!")
  }
}

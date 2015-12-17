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
      println("\nInput: " + file)
      println("  " + LineStream(Source fromFile file).toString.split("\n").mkString("\n  "))
      println("\nOutput:")
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
      //println("  " + results.head.map(s => s.asText).mkString(". "))
      try {
        val predicates = results.head.map(s => translate(s, NoContext))
        concludeOrValidate(predicates)
      } catch {
        case e:TranslationException =>
          println("  Translation failure: " + e.message)
      }
    } else {
      printf("  [warning] Ambiguity: %s valid English parses%n", results.length.toString)
      val goodresults:List[List[Predicate]] =
        results.flatMap{list => allExists[Predicate](list.map{sentence => try {
        Some(translate(sentence, NoContext))
      } catch {
        case e:TranslationException => None
      }})}.toList
      if (goodresults.length == 1) {
        println("  But there was only one surviving translation!\n")
        concludeOrValidate(goodresults.toList.head)
      } else if (goodresults.length == 0) {
        println("  [error] No parses survived translation. Here were the parses:")
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
      } else {
        println("  [error] Multiple translatable parses:")
        results.transpose.foreach{parses => {
          if (parses.toSet.size > 1) // parses are different
            if(parses.toSet.forall(s => try{
              translate(s, NoContext)
              true
            } catch {
              case e:TranslationException => false
            })) {
              // All alternatives are translatable
              println(parses.toSet.map{p:Sentence =>
                Utils.prettyprint(p)}.mkString("\n"))
          }
        else {}

        }}
        //goodresults.foreach(r => println("  " + r))
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
    println("  " + priors.mkString("\n  "))
    println("\n  Validating your conclusion by searching for counterexamples...")
    val numU = Conclusions.predictNumUniverses((priors ++ conclusions).toSet)
    if (numU > Math.pow(2, 22)) println("Too many possible universes to search through :(")
    else {
      val universes = Conclusions.allUniverses(
        Conclusions.extractEntities(priors ++ conclusions),
        Conclusions.extractRelations(priors ++ conclusions),
        Conclusions.extractBinaryRelations(priors ++ conclusions)).filter{ u =>
          priors.forall(_.evaluate(u))}
      conclusions.foreach(c => {
        val badu = universes.filter(!c.evaluate(_))
        if(badu.size == 0)
          println("  \033[32m✓\033[0m " + c + " (valid)")
        else
          println("  \033[31m✗\033[0m " + c + " (INVALID)\n  Counterexample: " + badu.head.toString.split("\n").mkString("\n  "))
    })}

  }
}

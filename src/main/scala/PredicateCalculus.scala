package semanticparser

import collection.mutable.Map
import AST._

object PredicateCalculus {
  object A {
    val dictionary = Map[Character, InA]()
    def apply(name:Character):Option[InA] = dictionary.get(name)
    def put(name:Character, value:InA) = dictionary.put(name, value)
    def relationVariable(word:String):Character = word.toUpperCase.filter{ c =>
      !dictionary.keys.filter{ x => Character.isUpperCase(x) }.exists{x => x ==c }}.head
    def entityVariable(word:String):Character = word.toLowerCase.filter{ c =>
      !dictionary.keys.filter{ x => Character.isLowerCase(x) }.exists{x => x == c }}.head
  }
  sealed trait InA
  case class Relation(members:Set[Entity]) extends InA
  case class Entity extends InA

  sealed trait Predicate
  case class Conjunction(a:Predicate,   b:Predicate) extends Predicate {
    override def toString = "(" + a.toString + " & " + b.toString + ")"
  }
  case class Disjunction(a:Predicate,   b:Predicate) extends Predicate {
    override def toString = "(" + a.toString + " | " + b.toString + ")"
  }
  case class Existential(v:Character,    p:Predicate) extends Predicate {
    // TODO override tostring for the rest...
  }
  case class Universal(v:Character,      p:Predicate) extends Predicate
  case class Conditional(a:Predicate,   b:Predicate) extends Predicate
  case class Biconditional(a:Predicate, b:Predicate) extends Predicate
  case class Atom(relation:Character,  variable:Character) extends Predicate {
    override def toString = relation.toString + variable.toString
  }

  def translate(sentence:Sentence):Predicate = sentence match {
    case Sentence(NP(None, Nbar(Left(Noun(n)), None)), VP(None, Vbar(Left(Verb(v)), None, None))) => Atom(A.relationVariable(v), A.entityVariable(n))
    case _ => Atom('N','O')
  }

}

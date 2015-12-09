package semanticparser

import collection.mutable.Map
import collection.mutable.Set
import XPrules._
import VPrules._
import NPrules._

object PredicateCalculus {
  class A {
    val entityMap = Map[EntityDesignation, Entity]()
    val relationMap = Map[RelationDesignation, Relation]()
    def apply(designation:EntityDesignation):Option[Entity] = entityMap get designation
    def apply(designation:RelationDesignation):Option[Relation] = relationMap get designation
    def put(e:Entity) = entityMap.put(e.designation, e)
    def put(r:Relation) = relationMap.put(r.designation, r)
  }
  object UniqueDesignations {
    val entities:Set[String] = Set[String]()
    val relations:Set[String] = Set[String]()
    var lastHypothetical:Character = ('a'.toInt - 1).toChar

    def relationDesignation(word:String):RelationConstant = {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations add newrel
      RelationConstant(newrel)
    }
    def entityDesignation(word:String):EntityConstant = {
      val newent = word.toLowerCase.filter{ c => !entities.contains(c.toString) }.head.toString
      entities add newent
      EntityConstant(newent)
    }
    def hypotheticalDesignation:EntityConstant = {
      lastHypothetical = (lastHypothetical.toInt + 1).toChar
      EntityConstant("hypothetical_" + lastHypothetical.toString)
    }
    def variableDesignation:EntityVariable = {
      val newvar = (('x' to 'z') ++ ('a' to 'w')).filter{
        x => !entities.contains(x.toString)}.head.toString
      entities add newvar
      EntityVariable(newvar)
    }
  }

  sealed trait InA
  case class Relation(designation:RelationDesignation, members:Set[Entity]) extends InA
  case class Entity(designation:EntityDesignation) extends InA

  sealed trait Designation
  sealed trait RelationDesignation extends Designation {
    val value:String
  }
  case class RelationConstant(value:String) extends RelationDesignation
  case class RelationVariable(value:String) extends RelationDesignation // Not used

  sealed trait EntityDesignation extends Designation {
    val value:String
  }
  case class EntityConstant(value:String) extends EntityDesignation
  case class EntityVariable(value:String) extends EntityDesignation

  val universes:Set[A] = Set[A]()

  sealed trait Predicate {
    def evaluate(universe:A): Boolean
    val relations:Set[Relation]
    val entities:Set[Entity]
  }
  case class Conjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) && b.evaluate(universe)
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " & " + b.toString + ")"
  }
  case class Disjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) || b.evaluate(universe)
    lazy val relations = a.relations ++ b.relations

    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " | " + b.toString + ")"
  }
  case class Existential(v:EntityVariable, p:Predicate) extends Predicate {
    //def evaluate = A.entities +  ^^ {e => p.replace(v, e).evaluate}.foldLeft(false, ||)
    def evaluate(universe:A) = false // TODO
    lazy val relations = p.relations
    lazy val entities = p.entities + Entity(UniqueDesignations.hypotheticalDesignation)
    override def toString = "(∃" + v.toString + ")" + p.toString
  }
  case class Universal(v:EntityVariable, p:Predicate) extends Predicate {
    //def evaluate(universe:A) = universe.entities ^^ {e => p.replace(v, e).evaluate}.foldLeft(true, &&)
    def evaluate(universe:A) = false // TODO
    lazy val relations = p.relations
    lazy val entities = p.entities // TODO is this right? I think so...
    override def toString = "(∀" + v.toString + ")" + p.toString
  }
  case class Conditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = b.evaluate(universe) || !a.evaluate(universe)
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " → " + b.toString + ")"
  }
  case class Biconditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) == b.evaluate(universe)
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ↔ " + b.toString + ")"
  }
  case class Atom(relation:RelationDesignation, entity:EntityDesignation) extends Predicate {
    def evaluate(universe:A) = entity match {
      case _:EntityVariable => false // TODO exception
      case c:EntityConstant => (universe(relation), universe(c)) match {
        case (Some(Relation(d,m)), Some(e)) => m contains e
        case _ => false
      }
    }
    // No members, despite this predicate asserting a membership:
    lazy val relations = Set[Relation](Relation(relation, Set()))
    lazy val entities = Set[Entity](Entity(entity))
    override def toString = relation.value + entity.value
  }
  case class NullPredicate extends Predicate {
    val entities = Set[Entity]()
    val relations = Set[Relation]()
    def evaluate(u:A) = false
  }

  /**
   * Add p to the knowledge of the world
   */
  //def incorporate(p: Predicate)

  def extractRelations(predicates:Set[Predicate]):Set[Relation] = predicates flatMap {p => p.relations}
  def extractEntities(predicates:Set[Predicate]):Set[Entity] = predicates flatMap {p => p.entities}


  def translate(sentence:Sentence):Predicate = sentence match {
    case VP(Some(NP(None, Nbar(Left(Noun(n)), None))), Vbar(Left(Verb(v)), None, None)) =>
      Atom(UniqueDesignations.relationDesignation(v), UniqueDesignations.entityDesignation(n))
    case _ => NullPredicate()
  }

}

package semanticparser

import collection.mutable.Map
import collection.mutable.{Set => MutSet}
import XPrules._
import VPrules._
import NPrules._
import DPrules._

object PredicateCalculus {
  class A {

    /** All entities/relations in this universe */
    val entities = MutSet[EntityConstant]()
    //val relationMap = Map[RelationDesignation, Relation]()
    val relations = MutSet[Relation]()

    /** Map (e,R) to the truth value of Re */
    val applicationMap = Map[(EntityConstant, Relation), Boolean]()

    /** Get the truth value of Re */
    def apply(e:EntityConstant, r:Relation):Boolean = applicationMap((e,r))

    def put(e:EntityConstant) = entities add e
    def put(r:Relation) = relations add r

    /** Assert that the entity is a member of the relation, or not */
    def relate(e:EntityConstant, r:Relation, b:Boolean) = {
      this.put(e)
      this.put(r)
      applicationMap.put((e,r), b)
    }

    override def toString = "\n[[Entities: " + entities.map(_.value).mkString(" ") + "\n" +
        (relations map {r => r.value + ": {" + (entities.filter{
          e => applicationMap((e, r))}.map{e => e.value}.mkString(" ")) +
        "}" + "\n"} mkString "") + "]]\n"
  }


  sealed trait Relation {
    val value:String
    def toEnglish = UniqueDesignations.name(this).getOrElse(value)
  }
  sealed trait Entity {
    val value:String
    def toEnglish:String
  }

  case class IsARelation(value:String) extends Relation
  case class DoesRelation(value:String) extends Relation

  case class EntityConstant(value:String) extends Entity {
    def toEnglish = UniqueDesignations.name(this).getOrElse(value)
  }
  case class EntityVariable(value:String) extends Entity {
    def toEnglish = value
  }

  sealed trait Predicate {
    def evaluate(universe:A): Boolean
    def replace(v:EntityVariable, e:EntityConstant):Predicate
    val relations:Set[Relation]
    val entities:Set[EntityConstant]
    def toEnglish:String
  }
  case class Conjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) && b.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Conjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " & " + b.toString + ")"
    def toEnglish = a.toEnglish + " and " + b.toEnglish
  }
  case class Disjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) || b.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Disjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations

    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " | " + b.toString + ")"
    def toEnglish = "either " + a.toEnglish + ", or " + b.toEnglish
  }
  case class Existential(v:EntityVariable, p:Predicate) extends Predicate {
    //def evaluate = A.entities +  ^^ {e => p.replace(v, e).evaluate}.foldLeft(false, ||)
    def evaluate(universe:A) = false // TODO
    def replace(v1:EntityVariable, e:EntityConstant):Predicate = Existential(v, p.replace(v1, e))
    lazy val relations = p.relations
    lazy val entities = p.entities + UniqueDesignations.hypotheticalDesignation + UniqueDesignations.hypotheticalDesignation
    override def toString = "(∃" + v.value + ")" + p.toString
    def toEnglish = "there is some entity " + v.value + " such that " + p.toEnglish
  }
  case class Universal(v:EntityVariable, p:Predicate) extends Predicate {
    //def evaluate(universe:A) = universe.entities ^^ {e => p.replace(v, e).evaluate}.foldLeft(true, &&)
    def evaluate(universe:A) = universe.entities.forall(e => p.replace(v,e).evaluate(universe))
    def replace(v1:EntityVariable, e:EntityConstant):Predicate = Universal(v, p.replace(v1,e))
    lazy val relations = p.relations
    lazy val entities = p.entities + UniqueDesignations.hypotheticalDesignation
    override def toString = "(∀" + v.value + ")" + p.toString
    def toEnglish = "for all entities " + v.value + ", " + p.toEnglish
  }
  case class Conditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = b.evaluate(universe) || !a.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Conditional(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " → " + b.toString + ")"
    def toEnglish = "if " + a.toEnglish + ", then " + b.toEnglish
  }
  case class Biconditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) == b.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Biconditional(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ↔ " + b.toString + ")"
    def toEnglish = a.toEnglish + " if and only if " + b.toString
  }
  case class Negation(p:Predicate) extends Predicate {
    def evaluate(universe:A) = !p.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Negation(p.replace(v,e))
    lazy val relations = p.relations
    lazy val entities = p.entities
    override def toString = "¬" + p.toString
    def toEnglish = p match {
      case Atom(i:IsARelation,e) => e.toEnglish + " isn't a " + i.toEnglish
      case Atom(d:DoesRelation,e) => e.toEnglish + " doesn't " + d.toEnglish
      case _ => "not " + p.toEnglish // lame
    }
  }
  case class Atom(relation:Relation, entity:Entity) extends Predicate {
    def evaluate(universe:A) = entity match {
      case _:EntityVariable => false // TODO exception
      case c:EntityConstant => universe(c,relation)
    }
    def replace(v:EntityVariable, e:EntityConstant):Predicate = entity match {
      case `v` => Atom(relation, e)
      case _ => this
    }
    lazy val relations = Set[Relation](relation)
    lazy val entities = entity match {
      case c:EntityConstant => Set[EntityConstant](c)
      case _ => Set[EntityConstant]()
    }
    override def toString = relation.value + entity.value
    def toEnglish = relation match {
      case r:IsARelation => entity.toEnglish + " is a " + r.toEnglish
      case r:DoesRelation => entity.toEnglish + " " + Verb(r.toEnglish,false).asText
    }
  }
  case class NullPredicate extends Predicate {
    val entities = Set[EntityConstant]()
    val relations = Set[Relation]()
    def evaluate(u:A) = false
    def replace(v:EntityVariable, e:EntityConstant) = this
    def toEnglish = "NULL PREDICATE"
  }

  def extractRelations(predicates:Set[Predicate]):Set[Relation] = predicates flatMap {p => p.relations}
  def extractEntities(predicates:Set[Predicate]):Set[EntityConstant] = predicates flatMap {p => p.entities}

  def allUniverses(e:Set[EntityConstant], r:Set[Relation]):Set[A] = {
    val entity = e.toIndexedSeq
    val relation = r.toIndexedSeq
    Utils.refeed[Set[List[Boolean]]](
      e.size * r.size, Utils.expandBitstring, Set[List[Boolean]](List())) map { bitstring =>
        val universe = new A()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
  }}

  def possibleUniverses(predicates:Set[Predicate]):Set[A] = allUniverses(
    extractEntities(predicates), extractRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def generateAtoms(e:Set[EntityConstant], r:Set[Relation]):Set[Predicate] = e flatMap {entity =>
    r flatMap {relation => List(Atom(relation, entity), Negation(Atom(relation, entity)))}}

  def generateUniversalConditionals(from:Set[Relation]):Set[Predicate] = from flatMap {antecedent =>
    from flatMap {consequent => antecedent match {
      case `consequent` => None
      case _ => {
        val v = UniqueDesignations.variableDesignation
        Some(Universal(v, Conditional(Atom(antecedent, v), Atom(consequent, v))))
      }
    }}}

  def validConclusions(predicates:Set[Predicate], universes:Set[A]):Set[Predicate] =
    predicates filter { p =>
      universes.forall(u => p.evaluate(u))}

  def isInteresting(conclusion:Predicate, priors:Set[Predicate]):Boolean = !priors.contains(conclusion)

  def generateConclusions(priors:Set[Predicate]):Set[Predicate] = {
    val universes:Set[A] = possibleUniverses(priors)
    val relations:Set[Relation] = universes.head.relations.toSet
    val entities:Set[EntityConstant] = universes.head.entities.toSet
    validConclusions(generateUniversalConditionals(relations) ++ generateAtoms(
      entities, relations), universes).filter {p =>
      isInteresting(p, priors)}
  }
}

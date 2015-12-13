package semanticparser

import collection.mutable.Map
import collection.mutable.{Set => MutSet}
import XPrules._
import VPrules._
import NPrules._
import DPrules._

object PredicateCalculus {
  class Universe {

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
    def evaluate(universe:Universe): Boolean
    def replace(v:EntityVariable, e:Entity):Predicate
    val relations:Set[Relation]
    val entities:Set[EntityConstant]
    def toEnglish:String
    def equivalent(other:Predicate):Boolean
  }
  case class Conjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:Universe) = a.evaluate(universe) && b.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Conjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " & " + b.toString + ")"
    def toEnglish = a.toEnglish + " and " + b.toEnglish
    def equivalent(other:Predicate) = other match {
      case Conjunction(a1, b1) => a.equivalent(a1) && b.equivalent(b1)
      case _ => false
    }
  }
  case class Disjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:Universe) = a.evaluate(universe) || b.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Disjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations

    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " | " + b.toString + ")"
    def toEnglish = "either " + a.toEnglish + ", or " + b.toEnglish
    def equivalent(other:Predicate) = other match {
      case Disjunction(a1, b1) => a.equivalent(a1) && b.equivalent(b1)
      case _ => false
    }
  }
  case class Existential(v:EntityVariable, p:Predicate) extends Predicate {
    def evaluate(universe:Universe) = universe.entities.exists(e => p.replace(v,e).evaluate(universe))
    def replace(v1:EntityVariable, e:Entity):Predicate = Existential(v, p.replace(v1, e))
    lazy val relations = p.relations
    lazy val entities = p.entities + UniqueDesignations.hypotheticalDesignation + UniqueDesignations.hypotheticalDesignation
    override def toString = "(∃" + v.value + ")" + p.toString
    def toEnglish = "there is some entity " + v.value + " such that " + p.toEnglish
    def equivalent(other:Predicate) = other match {
      case Existential(u,pr) => p.equivalent(pr)
      case _ => false
    }
  }
  case class Universal(v:EntityVariable, p:Predicate) extends Predicate {
    def evaluate(universe:Universe) = universe.entities.forall(e => p.replace(v,e).evaluate(universe))
    def replace(v1:EntityVariable, e:Entity):Predicate = Universal(v, p.replace(v1,e))
    lazy val relations = p.relations
    lazy val entities = p.entities + UniqueDesignations.hypotheticalDesignation
    override def toString = "(∀" + v.value + ")" + p.toString
    def toEnglish = "for all entities " + v.value + ", " + p.toEnglish
    def equivalent(other:Predicate) = other match {
      case Universal(u,pr) => p.equivalent(pr)
      case _ => false
    }
  }
  case class Conditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:Universe) = b.evaluate(universe) || !a.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Conditional(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " → " + b.toString + ")"
    def toEnglish = "if " + a.toEnglish + ", then " + b.toEnglish
    def equivalent(other:Predicate) = other match {
      case Conditional(a1, b1) => a.equivalent(a1) && b.equivalent(b1)
      case _ => false
    }
  }
  case class Biconditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:Universe) = a.evaluate(universe) == b.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Biconditional(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ↔ " + b.toString + ")"
    def toEnglish = a.toEnglish + " if and only if " + b.toString
    def equivalent(other:Predicate) = other match {
      case Biconditional(a1, b1) => a.equivalent(a1) && b.equivalent(b1)
      case _ => false
    }
  }
  case class Negation(p:Predicate) extends Predicate {
    def evaluate(universe:Universe) = !p.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Negation(p.replace(v,e))
    lazy val relations = p.relations
    lazy val entities = p.entities
    override def toString = "¬" + p.toString
    def toEnglish = p match {
      case Atom(i:IsARelation,e) => e.toEnglish + " isn't a " + i.toEnglish
      case Atom(d:DoesRelation,e) => e.toEnglish + " doesn't " + d.toEnglish
      case _ => "not " + p.toEnglish // lame
    }
    def equivalent(other:Predicate) = other match {
      case Negation(a1) => p.equivalent(a1)
      case _ => false
    }
  }
  case class Atom(relation:Relation, entity:Entity) extends Predicate {
    def evaluate(universe:Universe) = entity match {
      case _:EntityVariable => throw new RuntimeException("Cannot evaluate an atom containing a variable")
      case c:EntityConstant => universe(c,relation)
    }
    def replace(v:EntityVariable, e:Entity):Predicate = entity match {
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
    def equivalent(other:Predicate) = other match {
      case Atom(r1, e1) => r1.equals(relation) && (e1 match {
        case `entity` => true
        case e:EntityVariable => entity match {
          case _:EntityVariable => true
          case _ => false
        }
        case _ => false
      })
      case _ => false
    }
  }

  def extractRelations(predicates:Set[Predicate]):Set[Relation] = predicates flatMap {p => p.relations}
  def extractEntities(predicates:Set[Predicate]):Set[EntityConstant] = predicates flatMap {p => p.entities}

  def allUniverses(e:Set[EntityConstant], r:Set[Relation]):Set[Universe] = {
    val entity = e.toIndexedSeq
    val relation = r.toIndexedSeq
    Utils.refeed[Set[List[Boolean]]](
      e.size * r.size, Utils.expandBitstring, Set[List[Boolean]](List())) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
  }}

  def possibleUniverses(predicates:Set[Predicate]):Set[Universe] = allUniverses(
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
        List(Universal(v, Conditional(Atom(antecedent, v), Atom(consequent, v))),
          Universal(v, Conditional(Negation(Atom(antecedent, v)), Atom(consequent, v))),
          Universal(v, Conditional(Atom(antecedent, v), Negation(Atom(consequent, v)))),
          Universal(v, Conditional(Negation(Atom(antecedent, v)), Negation(Atom(consequent, v)))))
      }
    }}}

  def validConclusions(predicates:Set[Predicate], universes:Set[Universe]):Set[Predicate] =
    predicates filter { p =>
      universes.forall(u => p.evaluate(u))}

  def isInteresting(conclusion:Predicate, priors:Set[Predicate]):Boolean =
    !priors.contains(conclusion) && priors.forall(!_.equivalent(conclusion)) &&
    priors.forall(p =>
        !generateConclusions(priors - p).contains(conclusion) &&
        !generateConclusions(priors - p).exists(_.equivalent(conclusion)))

  def generateConclusions(priors:Set[Predicate]):Set[Predicate] = {
    if (priors.size == 0) return Set()
    val universes:Set[Universe] = possibleUniverses(priors)
    if (universes.size == 0) return Set()
    val relations:Set[Relation] = universes.head.relations.toSet
    val entities:Set[EntityConstant] = universes.head.entities.toSet
    validConclusions(generateUniversalConditionals(relations) ++ generateAtoms(
      entities, relations), universes).filter {p =>
      isInteresting(p, priors)}
  }
}

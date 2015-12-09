package semanticparser

import collection.mutable.Map
import collection.mutable.{Set => MutSet}
import XPrules._
import VPrules._
import NPrules._

object PredicateCalculus {
  class A {

    /** All entities/relations in this universe */
    val entityMap = Map[EntityDesignation, Entity]()
    //val relationMap = Map[RelationDesignation, Relation]()
    val relations = MutSet[Relation]()

    /** Map (e,R) to the truth value of Re */
    val applicationMap = Map[(Entity, Relation), Boolean]()

    /** Get an Entity/Relation from its Designation **/
    def apply(designation:EntityDesignation):Option[Entity] = entityMap get designation
    def apply(relation:Relation):Option[Relation] = relations contains relation match {
      case true => Some(relation)
      case false => None
    }//TODO how is this even used

    /** Get the truth value of Re */
    def apply(e:Entity, r:Relation):Boolean = applicationMap((e,r))

    def put(e:Entity) = entityMap.put(e.designation, e)
    def put(r:Relation) = relations add r

    /** Assert that the entity is a member of the relation, or not */
    def relate(e:Entity, r:Relation, b:Boolean) = {
      this.put(e)
      this.put(r)
      applicationMap.put((e,r), b)
    }

    override def toString = applicationMap.toString
  }
  object UniqueDesignations {
    val entities:MutSet[String] = MutSet[String]()
    val relations:MutSet[String] = MutSet[String]()
    var lastHypothetical:Character = ('a'.toInt - 1).toChar

    def doesRelation(word:String):Relation = {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations add newrel
      DoesRelation(newrel)
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
  sealed trait Relation extends InA {
    val value:String
  }
  case class Entity(designation:EntityDesignation) extends InA

  case class IsARelation(value:String) extends Relation
  case class DoesRelation(value:String) extends Relation

  sealed trait Designation
  //sealed trait RelationDesignation extends Designation {
  //  val value:String
  //}
  //case class RelationConstant(value:String) extends RelationDesignation
  //case class RelationVariable(value:String) extends RelationDesignation // Not used

  sealed trait EntityDesignation extends Designation {
    val value:String
  }
  case class EntityConstant(value:String) extends EntityDesignation
  case class EntityVariable(value:String) extends EntityDesignation

  //val universes:Set[A] = Set[A]()

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
  case class Negation(p:Predicate) extends Predicate {
    def evaluate(universe:A) = !p.evaluate(universe)
    lazy val relations = p.relations
    lazy val entities = p.entities
    override def toString = "¬" + p.toString
  }
  case class Atom(relation:Relation, entity:EntityDesignation) extends Predicate {
    def evaluate(universe:A) = entity match {
      case _:EntityVariable => false // TODO exception
      case c:EntityConstant => universe(c) match {
        case Some(e) => universe(e,relation)
        case _ => throw new RuntimeException("Tried to evaluate nonexistent designation")//TODO whyyy
      }
    }
    lazy val relations = Set[Relation](relation)
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

  def allUniverses(e:Set[Entity], r:Set[Relation]):Set[A] = {
    val entity = e.toIndexedSeq
    val relation = r.toIndexedSeq
    //println(Utils.refeed[Int](10, x=>x+1, 0))
    //println(e.size*r.size)
    //println(Utils.refeed(2, Utils.expandBitstring, Set[List[Boolean]](List())))
    Utils.refeed[Set[List[Boolean]]](
      e.size * r.size, Utils.expandBitstring, Set[List[Boolean]](List())) map { bitstring =>
        val universe = new A()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
  }}

  def possibleUniverses(predicates:Set[Predicate]) = allUniverses(
    extractEntities(predicates), extractRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def translate(sentence:Sentence):Predicate = sentence match {
    case VP(Some(NP(None, Left(Nbar(Left(Noun(n)), None)))), Left(Vbar(Left(Verb(v)), None, None))) =>
      Atom(UniqueDesignations.doesRelation(v), UniqueDesignations.entityDesignation(n))
    case _ => NullPredicate()
  }

}

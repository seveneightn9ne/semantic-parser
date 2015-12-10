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

    /** Get an Entity/Relation from its Designation **/
    //def apply(entity:Entity):Option[Entity] = entityMap get designation
    //def apply(relation:Relation):Option[Relation] = relations contains relation match {
    //  case true => Some(relation)
    //  case false => None
    //}//TODO how is this even used

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

    override def toString = "\n[[Entities: " + entities.map(_.value).mkString(" ") + "\n" + (relations map {r =>
      r.value + ": {" + (entities.filter{e => applicationMap((e, r))}.map{e => e.value}.mkString(" ")) + "}" + "\n"} mkString "") + "]]\n"
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
    def isARelation(word:String):Relation = {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations add newrel
      IsARelation(newrel)
    }
    def entityConstant(word:String):EntityConstant = {
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
  sealed trait Entity extends InA {
    val value:String
  }

  case class IsARelation(value:String) extends Relation
  case class DoesRelation(value:String) extends Relation

  //sealed trait Designation
  //sealed trait RelationDesignation extends Designation {
  //  val value:String
  //}
  //case class RelationConstant(value:String) extends RelationDesignation
  //case class RelationVariable(value:String) extends RelationDesignation // Not used

  //sealed trait EntityDesignation extends Designation {
  //  val value:String
  //}
  case class EntityConstant(value:String) extends Entity
  case class EntityVariable(value:String) extends Entity

  //val universes:Set[A] = Set[A]()

  sealed trait Predicate {
    def evaluate(universe:A): Boolean
    def replace(v:EntityVariable, e:EntityConstant):Predicate
    val relations:Set[Relation]
    val entities:Set[EntityConstant]
  }
  case class Conjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) && b.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Conjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " & " + b.toString + ")"
  }
  case class Disjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) || b.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Disjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations

    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " | " + b.toString + ")"
  }
  case class Existential(v:EntityVariable, p:Predicate) extends Predicate {
    //def evaluate = A.entities +  ^^ {e => p.replace(v, e).evaluate}.foldLeft(false, ||)
    def evaluate(universe:A) = false // TODO
    def replace(v1:EntityVariable, e:EntityConstant):Predicate = Existential(v, p.replace(v1, e))
    lazy val relations = p.relations
    lazy val entities = p.entities + UniqueDesignations.hypotheticalDesignation + UniqueDesignations.hypotheticalDesignation
    override def toString = "(∃" + v.value + ")" + p.toString
  }
  case class Universal(v:EntityVariable, p:Predicate) extends Predicate {
    //def evaluate(universe:A) = universe.entities ^^ {e => p.replace(v, e).evaluate}.foldLeft(true, &&)
    def evaluate(universe:A) = universe.entities.forall(e => p.replace(v,e).evaluate(universe))
    def replace(v1:EntityVariable, e:EntityConstant):Predicate = Universal(v, p.replace(v1,e))
    lazy val relations = p.relations
    lazy val entities = p.entities + UniqueDesignations.hypotheticalDesignation
    override def toString = "(∀" + v.value + ")" + p.toString
  }
  case class Conditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = b.evaluate(universe) || !a.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Conditional(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " → " + b.toString + ")"
  }
  case class Biconditional(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:A) = a.evaluate(universe) == b.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Biconditional(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ↔ " + b.toString + ")"
  }
  case class Negation(p:Predicate) extends Predicate {
    def evaluate(universe:A) = !p.evaluate(universe)
    def replace(v:EntityVariable, e:EntityConstant):Predicate = Negation(p.replace(v,e))
    lazy val relations = p.relations
    lazy val entities = p.entities
    override def toString = "¬" + p.toString
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
  }
  case class NullPredicate extends Predicate {
    val entities = Set[EntityConstant]()
    val relations = Set[Relation]()
    def evaluate(u:A) = false
    def replace(v:EntityVariable, e:EntityConstant) = this
  }

  /**
   * Add p to the knowledge of the world
   */
  //def incorporate(p: Predicate)

  def extractRelations(predicates:Set[Predicate]):Set[Relation] = predicates flatMap {p => p.relations}
  def extractEntities(predicates:Set[Predicate]):Set[EntityConstant] = predicates flatMap {p => p.entities}

  def allUniverses(e:Set[EntityConstant], r:Set[Relation]):Set[A] = {
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
        //println(bitstring)
        //println(universe)
        universe
  }}

  def possibleUniverses(predicates:Set[Predicate]) = allUniverses(
    extractEntities(predicates), extractRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def translate(sentence:Sentence):Predicate = sentence match {

    // Simple intransitive, e.g. "John eats" -> Ej
    case VP(
      Some(NP(None,
        Left(Nbar(
          Left(Noun(n)), None)))),
      Left(Vbar(
        Left(Verb(v)), None, None))) =>

      Atom(
        UniqueDesignations.doesRelation(v),
        UniqueDesignations.entityConstant(n))

    // Universal with "every", e.g. "Every man eats" -> (∀x)(Mx → Ex)
    case VP(
      Some(NP(
        Some(DP(None,
          Left(Dbar(Left(Determiner(DValues.Every)))))),
        Left(Nbar(
          Left(Noun(n)), None)))),
      Left(Vbar(
        Left(Verb(v)), None, None))) => {

      val variable = UniqueDesignations.variableDesignation
      Universal(variable,
        Conditional(
          Atom(UniqueDesignations.isARelation(n), variable),
          Atom(UniqueDesignations.isARelation(v), variable)))
    }
    case _ => NullPredicate()
  }

}

package semanticparser

import Predicates._
import collection.mutable.Map
import collection.mutable.{Set => MutSet}

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

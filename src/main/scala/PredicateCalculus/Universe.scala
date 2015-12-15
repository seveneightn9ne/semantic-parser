package semanticparser

import Predicates._
import collection.mutable.Map
import collection.mutable.{Set => MutSet}

class Universe {

  /** All entities/relations in this universe */
  val entities = MutSet[EntityConstant]()
  val unaryRelations = MutSet[UnaryRelation]()
  val binaryRelations = MutSet[BinaryRelation]()
  //def relations = onePlaceRelations ++ twoPlaceRelations

  /** Map (e,R) to the truth value of Re */
  val applicationMap = Map[(EntityConstant, UnaryRelation), Boolean]()

  /** Map (e,f,R) to the truth value of R(e,f) */
  val twoPlacePredicates = Map[(EntityConstant, EntityConstant, BinaryRelation), Boolean]()

  /** Get the truth value of Re */
  def apply(e:EntityConstant, r:UnaryRelation):Boolean = applicationMap((e,r))
  def apply(e:EntityConstant, f:EntityConstant, r:BinaryRelation):Boolean = twoPlacePredicates((e,f,r))

  def put(e:EntityConstant) = entities add e
  def put(r:UnaryRelation) = unaryRelations add r
  def put(r:BinaryRelation) = binaryRelations add r

  /** Assert that the entity is a member of the relation, or not */
  def relate(e:EntityConstant, r:UnaryRelation, b:Boolean) = {
    this.put(e)
    this.put(r)
    applicationMap.put((e,r), b)
  }
  def relate(e:EntityConstant, f:EntityConstant, r:BinaryRelation, b:Boolean) = {
    this.put(e)
    this.put(f)
    this.put(r)
    twoPlacePredicates.put((e,f,r), b)
  }

  override def toString = "\n[[Entities: " + entities.map(_.value).mkString(" ") + "\n" +
      (unaryRelations map {r => r.value + ": {" + (entities.filter{
        e => applicationMap((e, r))}.map{e => e.value}.mkString(" ")) +
      "}" + "\n"} mkString "") +
      (binaryRelations map {r => r.value + ": {" + (twoPlacePredicates.filter{t => t._2}.map{t =>
        "(" + t._1._1.value + ", " + t._1._2.value + ")"}.mkString(" ")) + "}\n"} mkString "") + "]]\n"
}

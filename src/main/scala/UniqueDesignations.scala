package semanticparser
import PredicateCalculus._
import collection.mutable.Map

object UniqueDesignations {
  val entities:Map[String,String] = Map[String,String]()
  //val relations:MutSet[String] = MutSet[String]()
  val relations:Map[String,String] = Map[String,String]()

  def name(r:Relation):Option[String] = relations map {_.swap} get r.value
  def name(e:EntityConstant):Option[String] = entities map {_.swap} get e.value
  var lastHypothetical:Character = ('a'.toInt - 1).toChar

  def doesRelation(word:String):Relation = relations get word match {
    case Some(rel) => DoesRelation(rel)
    case _ => {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations.put(word,newrel)
      DoesRelation(newrel)
    }
  }
  def isARelation(word:String):Relation = relations get word match {
    case Some(rel) => IsARelation(rel)
    case _ => {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations.put(word,newrel)
      IsARelation(newrel)
    }
  }
  def entityConstant(word:String):EntityConstant = entities get word match {
    case Some(ent) => EntityConstant(ent)
    case _ => {
      val newent = word.toLowerCase.filter{ c => !entities.contains(c.toString) }.head.toString
      entities.put(word,newent)
      EntityConstant(newent)
    }
  }
  def hypotheticalDesignation:EntityConstant = {
    lastHypothetical = (lastHypothetical.toInt + 1).toChar
    EntityConstant("hypothetical_" + lastHypothetical.toString)
  }
  def variableDesignation:EntityVariable = {
    val newvar = (('x' to 'z') ++ ('a' to 'w')).filter{
      x => !entities.values.toSet.contains(x.toString)}.head.toString
    entities.put(newvar,newvar)
    EntityVariable(newvar)
  }
}

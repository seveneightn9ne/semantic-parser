package semanticparser
import Predicates._
import Translation._
import collection.mutable.Map

object UniqueDesignations {
  val entities:Map[String,String] = Map[String,String]()
  val relations:Map[String,String] = Map[String,String]()

  def name(r:Relation):Option[String] = relations map {_.swap} get r.value
  def name(e:EntityConstant):Option[String] = entities map {_.swap} get e.value
  var lastHypothetical:Character = ('a'.toInt - 1).toChar

  def doesRelation(word:String):UnaryRelation = relations get word match {
    case Some(rel) => DoesRelation(rel)
    case _ => {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations.put(word,newrel)
      DoesRelation(newrel)
    }
  }
  def isARelation(word:String):UnaryRelation = relations get word match {
    case Some(rel) => IsARelation(rel)
    case _ => {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations.put(word,newrel)
      IsARelation(newrel)
    }
  }
  def binaryRelation(word:String):BinaryRelation = relations get word match {
    case Some(rel) => BinaryRelation(rel)
    case _ => {
      val newrel = word.toUpperCase.filter{ c => !relations.contains(c.toString) }.head.toString
      relations.put(word,newrel)
      BinaryRelation(newrel)
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
  lazy val possibleVariables = ('x' to 'z') ++ ('a' to 'w')
  def variableDesignation(ctx:Context):EntityVariable = {
    val newvar = possibleVariables.filter{x => ctx match {
      case Subject(e) => e.value != x.toString
      case SubjectPredicate(e, _) => e.value != x.toString
      case _ => true
    }}.head.toString
    entities.put(newvar,newvar)
    EntityVariable(newvar)
  }
}

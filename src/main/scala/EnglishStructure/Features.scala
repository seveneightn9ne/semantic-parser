package semanticparser
import VPrules.VP
import NPrules._
import XPrules._
object Features{
  trait Feature {
    val value: Option[Boolean]
    //val intrinsic:Boolean
    //val epp: Boolean
    def valued = value.isDefined
    override def toString = value match {
      case Some(true) => "[+" + this.getClass.getSimpleName + "]"
      case Some(false) => "[-" + this.getClass.getSimpleName + "]"
      case None => "[u" + this.getClass.getSimpleName + "]"
    }
  }

  /*sealed trait TenseVal
  case object Present extends TenseVal
  case object Past extends TenseVal
  case object Future extends TenseVal
  case object Infinitive extends TenseVal
  class Tense(value: Option[TenseVal]) extends Feature[TenseVal] {
    val EPP = false
  }*/

  case class Plural(value:Option[Boolean]) extends Feature
  case class Wh(value:Option[Boolean])     extends Feature
  case class Rel(value:Option[Boolean])    extends Feature

}


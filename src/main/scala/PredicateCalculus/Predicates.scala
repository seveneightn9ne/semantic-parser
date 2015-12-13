package semanticparser

object Predicates {

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
      case r:DoesRelation => entity.toEnglish + " " + VPrules.Verb(r.toEnglish,false).asText
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
}

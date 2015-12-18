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

  sealed trait UnaryRelation extends Relation
  case class IsARelation(value:String) extends UnaryRelation
  case class DoesRelation(value:String) extends UnaryRelation
  case class BinaryRelation(value:String) extends Relation

  case class EntityConstant(value:String) extends Entity {
    def toEnglish = UniqueDesignations.name(this).getOrElse(value)
  }
  case class EntityVariable(value:String) extends Entity {
    def toEnglish = value
  }

  sealed trait Predicate {
    def evaluate(universe:Universe): Boolean
    def replace(v:EntityVariable, e:Entity):Predicate
    val relations:Set[UnaryRelation]
    val binaryRelations:Set[BinaryRelation]
    val entities:Set[EntityConstant]
    def toEnglish:String
    def equivalent(other:Predicate):Boolean
  }
  case class Conclusion(p:Predicate) extends Predicate {
    def evaluate(universe:Universe) = p.evaluate(universe)
    def replace(v:EntityVariable, e:Entity) = p.replace(v,e)
    val relations = p.relations
    val binaryRelations = p.binaryRelations
    val entities = p.entities
    def toEnglish = "Therefore, " + p.toEnglish
    def equivalent(other:Predicate) = other match {
      case Conclusion(q) => p.equivalent(q)
      case _ => false
    }
    override def toString = "∴ " + p.toString
  }
  case class Conjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:Universe) = a.evaluate(universe) && b.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Conjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val binaryRelations = a.binaryRelations ++ b.binaryRelations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ∧ " + b.toString + ")"
    def toEnglish = a.toEnglish + " and " + b.toEnglish
    def equivalent(other:Predicate) = other match {
      case Conjunction(a1, b1) =>
        (a.equivalent(a1) && b.equivalent(b1)) ||
        (a.equivalent(b1) && b.equivalent(a1))
      case _ => false
    }
  }
  case class Disjunction(a:Predicate, b:Predicate) extends Predicate {
    def evaluate(universe:Universe) = a.evaluate(universe) || b.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Disjunction(a.replace(v,e), b.replace(v,e))
    lazy val relations = a.relations ++ b.relations
    lazy val binaryRelations = a.binaryRelations ++ b.binaryRelations

    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ∨ " + b.toString + ")"
    def toEnglish = "either " + a.toEnglish + ", or " + b.toEnglish
    def equivalent(other:Predicate) = other match {
      case Disjunction(a1, b1) =>
        (a.equivalent(a1) && b.equivalent(b1)) ||
        (a.equivalent(b1) && b.equivalent(a1))
      case _ => false
    }
  }
  case class Existential(v:EntityVariable, p:Predicate) extends Predicate {
    def evaluate(universe:Universe) = universe.entities.exists(e => p.replace(v,e).evaluate(universe))
    def replace(v1:EntityVariable, e:Entity):Predicate = Existential(v, p.replace(v1, e))
    lazy val relations = p.relations
    lazy val binaryRelations = p.binaryRelations
    lazy val entities = p.entities
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
    lazy val binaryRelations = p.binaryRelations
    lazy val entities = p.entities
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
    lazy val binaryRelations = a.binaryRelations ++ b.binaryRelations
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
    lazy val binaryRelations = a.binaryRelations ++ b.binaryRelations
    lazy val entities = a.entities ++ b.entities
    override def toString = "(" + a.toString + " ↔ " + b.toString + ")"
    def toEnglish = a.toEnglish + " if and only if " + b.toString
    def equivalent(other:Predicate) = other match {
      case Biconditional(a1, b1) =>
        (a.equivalent(a1) && b.equivalent(b1)) ||
        (a.equivalent(b1) && b.equivalent(a1))
      case _ => false
    }
  }
  case class Negation(p:Predicate) extends Predicate {
    def evaluate(universe:Universe) = !p.evaluate(universe)
    def replace(v:EntityVariable, e:Entity):Predicate = Negation(p.replace(v,e))
    lazy val relations = p.relations
    lazy val binaryRelations = p.binaryRelations
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
  case class Atom(relation:UnaryRelation, entity:Entity) extends Predicate {
    def evaluate(universe:Universe) = entity match {
      case _:EntityVariable => throw new RuntimeException("Cannot evaluate an atom containing a variable")
      case c:EntityConstant => universe(c,relation)
    }
    def replace(v:EntityVariable, e:Entity):Predicate = entity match {
      case `v` => Atom(relation, e)
      case _ => this
    }
    lazy val relations = Set[UnaryRelation](relation)
    lazy val binaryRelations = Set[BinaryRelation]()
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
  case class BinaryAtom(relation:BinaryRelation, e:Entity, f:Entity) extends Predicate {
    def evaluate(universe:Universe) = (e,f) match {
      case (c:EntityConstant,d:EntityConstant) => universe(c,d,relation)
      case _ =>
        throw new RuntimeException("Cannot evaluate an atom containing a variable")
    }
    def replace(v:EntityVariable, entity:Entity):Predicate = v match {
      case `e` => BinaryAtom(relation, entity, f)
      case `f` => BinaryAtom(relation, e, entity)
      case _ => this
    }
    lazy val relations = Set[UnaryRelation]()
    lazy val binaryRelations = Set[BinaryRelation](relation)
    lazy val entities = (e,f) match {
      case (c:EntityConstant, d:EntityConstant) => Set[EntityConstant](c,d)
      case (_, d:EntityConstant) => Set[EntityConstant](d)
      case (c:EntityConstant, _) => Set[EntityConstant](c)
      case _ => Set[EntityConstant]()
    }
    override def toString = relation.value + e.value + f.value
    def toEnglish = e.toEnglish + " " + VPrules.Verb(relation.toEnglish, false).asText +
        " " + f.toEnglish
    def equivalent(other:Predicate) = other match {
      case BinaryAtom(r1, e1, f1) => r1.equals(relation) && ((e1, f1) match {
        case (`e`,`f`) => true
        case (`e`, _:EntityVariable) => f match {
          case _:EntityVariable => true
          case _ => false
        }
        case (_:EntityVariable, `f`) => e match {
          case _:EntityVariable => true
          case _ => false
        }
        case (_:EntityVariable, _:EntityVariable) => (e,f) match {
          case (_:EntityVariable, _:EntityVariable) => true
          case _ => false
        }
        case _ => false
      })
      case _ => false
    }
  }
}

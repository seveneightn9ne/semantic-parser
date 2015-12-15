package semanticparser

import Predicates._

object Conclusions {

  def extractRelations(predicates:Set[Predicate]):Set[UnaryRelation] = predicates flatMap {p => p.relations}
  def extractBinaryRelations(predicates:Set[Predicate]):Set[BinaryRelation] =
    predicates flatMap {p => p.binaryRelations}
  def extractEntities(predicates:Set[Predicate]):Set[EntityConstant] = predicates flatMap {p => p.entities}

  def allUniverses(e:Set[EntityConstant], r:Set[UnaryRelation], r2:Set[BinaryRelation]):List[Universe] = {
    val entity = e.toIndexedSeq
    val relation = r.toIndexedSeq
    val universes = Utils.refeed[List[List[Boolean]]](
      e.size * r.size, Utils.expandBitstring, List[List[Boolean]](List())) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
      }
    val binaryRelation = r2.toIndexedSeq
    val binaryUniverses = Utils.refeed[List[List[Boolean]]](
      e.size * e.size * r2.size, Utils.expandBitstring, List[List[Boolean]](List())) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), entity((i/e.size)%e.size),
            binaryRelation(i/(e.size*e.size)), bitstring(i))
        }
        universe
    }
    binaryUniverses flatMap{ bu => universes map { u =>
      val combinedUniverse = new Universe()
      combinedUniverse.entities ++= bu.entities
      combinedUniverse.entities ++= u.entities
      combinedUniverse.unaryRelations ++= u.unaryRelations
      combinedUniverse.binaryRelations ++= bu.binaryRelations
      combinedUniverse.applicationMap ++= u.applicationMap
      combinedUniverse.twoPlacePredicates ++= bu.twoPlacePredicates
      combinedUniverse
    }}
  }

  def possibleUniverses(predicates:Set[Predicate]):List[Universe] = allUniverses(
    extractEntities(predicates),
    extractRelations(predicates),
    extractBinaryRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def generateAtoms(e:Set[EntityConstant], r:Set[UnaryRelation]):Set[Predicate] = e flatMap {entity =>
    r flatMap {relation => List(Atom(relation, entity), Negation(Atom(relation, entity)))}}

  def generateUniversalConditionals(from:Set[UnaryRelation]):Set[Predicate] = from flatMap {antecedent =>
    from flatMap {consequent => antecedent match {
      case `consequent` => None
      case _ => {
        val v = UniqueDesignations.variableDesignation(Translation.NoContext)
        List(Universal(v, Conditional(Atom(antecedent, v), Atom(consequent, v))),
          Universal(v, Conditional(Negation(Atom(antecedent, v)), Atom(consequent, v))),
          Universal(v, Conditional(Atom(antecedent, v), Negation(Atom(consequent, v)))),
          Universal(v, Conditional(Negation(Atom(antecedent, v)), Negation(Atom(consequent, v)))))
      }
    }}}
  def generateExistentials(from:Set[UnaryRelation]):Set[Predicate] = from map {relation => {
    val v = UniqueDesignations.variableDesignation(Translation.NoContext)
    Existential(v, Atom(relation, v))
  }}

  def generateUniversals(from:Set[UnaryRelation]):Set[Predicate] = from map {relation => {
    val v = UniqueDesignations.variableDesignation(Translation.NoContext)
    Universal(v, Atom(relation, v))
  }}

  def validConclusions(predicates:Set[Predicate], universes:List[Universe]):Set[Predicate] =
    predicates filter { p =>
      universes.forall(u => p.evaluate(u))}

  def isInteresting(conclusion:Predicate, priors:Set[Predicate]):Boolean =
    !priors.contains(conclusion) && priors.forall(!_.equivalent(conclusion)) &&
    priors.forall(p =>
        !generateConclusions(Set(p)).contains(conclusion) &&
        !generateConclusions(Set(p)).exists(_.equivalent(conclusion))) &&
    !conclusion.toEnglish.contains("hypothetical_")

  def generateConclusions(priors:Set[Predicate]):Set[Predicate] = {
    if (priors.size == 0) return Set()
    val universes:List[Universe] = possibleUniverses(priors)
    if (universes.size == 0) return Set()
    val relations:Set[UnaryRelation] = universes.head.unaryRelations.toSet
    val entities:Set[EntityConstant] = universes.head.entities.toSet
    validConclusions(generateUniversalConditionals(relations) ++ generateAtoms(
      entities, relations) ++ generateExistentials(relations) ++ generateUniversals(relations),
    universes)
  }

  def generateInterestingConclusions(priors:Set[Predicate]):Set[Predicate] =
    generateConclusions(priors).filter {p => isInteresting(p, priors)}


}

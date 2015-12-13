package semanticparser

import Predicates._

object Conclusions {

  def extractRelations(predicates:Set[Predicate]):Set[Relation] = predicates flatMap {p => p.relations}
  def extractEntities(predicates:Set[Predicate]):Set[EntityConstant] = predicates flatMap {p => p.entities}

  def allUniverses(e:Set[EntityConstant], r:Set[Relation]):Set[Universe] = {
    val entity = e.toIndexedSeq
    val relation = r.toIndexedSeq
    Utils.refeed[Set[List[Boolean]]](
      e.size * r.size, Utils.expandBitstring, Set[List[Boolean]](List())) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
  }}

  def possibleUniverses(predicates:Set[Predicate]):Set[Universe] = allUniverses(
    extractEntities(predicates), extractRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def generateAtoms(e:Set[EntityConstant], r:Set[Relation]):Set[Predicate] = e flatMap {entity =>
    r flatMap {relation => List(Atom(relation, entity), Negation(Atom(relation, entity)))}}

  def generateUniversalConditionals(from:Set[Relation]):Set[Predicate] = from flatMap {antecedent =>
    from flatMap {consequent => antecedent match {
      case `consequent` => None
      case _ => {
        val v = UniqueDesignations.variableDesignation
        List(Universal(v, Conditional(Atom(antecedent, v), Atom(consequent, v))),
          Universal(v, Conditional(Negation(Atom(antecedent, v)), Atom(consequent, v))),
          Universal(v, Conditional(Atom(antecedent, v), Negation(Atom(consequent, v)))),
          Universal(v, Conditional(Negation(Atom(antecedent, v)), Negation(Atom(consequent, v)))))
      }
    }}}

  def validConclusions(predicates:Set[Predicate], universes:Set[Universe]):Set[Predicate] =
    predicates filter { p =>
      universes.forall(u => p.evaluate(u))}

  def isInteresting(conclusion:Predicate, priors:Set[Predicate]):Boolean =
    !priors.contains(conclusion) && priors.forall(!_.equivalent(conclusion)) &&
    priors.forall(p =>
        !generateConclusions(priors - p).contains(conclusion) &&
        !generateConclusions(priors - p).exists(_.equivalent(conclusion)))

  def generateConclusions(priors:Set[Predicate]):Set[Predicate] = {
    if (priors.size == 0) return Set()
    val universes:Set[Universe] = possibleUniverses(priors)
    if (universes.size == 0) return Set()
    val relations:Set[Relation] = universes.head.relations.toSet
    val entities:Set[EntityConstant] = universes.head.entities.toSet
    validConclusions(generateUniversalConditionals(relations) ++ generateAtoms(
      entities, relations), universes).filter {p =>
      isInteresting(p, priors)}
  }

}

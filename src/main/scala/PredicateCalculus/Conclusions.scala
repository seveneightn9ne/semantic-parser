package semanticparser

import Predicates._

object Conclusions {

  def extractRelations(predicates:Iterable[Predicate]):Set[UnaryRelation] =
    predicates.flatMap{p => p.relations}.toSet
  def extractBinaryRelations(predicates:Iterable[Predicate]):Set[BinaryRelation] =
    predicates.flatMap{p => p.binaryRelations}.toSet
  def extractEntities(predicates:Iterable[Predicate]):Set[EntityConstant] =
    (predicates.flatMap{p => p.entities}.toSet +
    UniqueDesignations.hypotheticalDesignation +
    UniqueDesignations.hypotheticalDesignation)

  def predictNumUniverses(predicates:Set[Predicate]) = {
    val e = extractEntities(predicates).size
    val r1 = extractRelations(predicates).size
    val r2 = extractBinaryRelations(predicates).size
    Math.pow(2, e*r1)*Math.pow(2, e*e*r2)
  }

  def allUniverses(e:Set[EntityConstant], r:Set[UnaryRelation], r2:Set[BinaryRelation]):List[Universe] = {
    val entity = e.toIndexedSeq
    val relation = r.toIndexedSeq
    val universes = Utils.bitstrings(e.size * r.size).map{ bitstring:IndexedSeq[Boolean] => {
        val universe = new Universe()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
    }}
    val binaryRelation = r2.toIndexedSeq
    val binaryUniverses = Utils.bitstrings(e.size * e.size * r2.size) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i => {
          universe.relate(entity(i % e.size), entity((i/e.size)%e.size),
            binaryRelation(i/(e.size*e.size)), bitstring(i))
        }}
        universe
    }
    binaryUniverses.flatMap{ bu => universes map { u => combineUniverses(List(bu, u)) }}.toList
  }
  def combineUniverses(us:List[Universe]):Universe = {
    val u = new Universe()
    us foreach { u1 =>
      u.entities ++= u1.entities
      u.unaryRelations ++= u1.unaryRelations
      u.binaryRelations ++= u1.binaryRelations
      u.applicationMap ++= u1.applicationMap
      u.twoPlacePredicates ++= u1.twoPlacePredicates
    }
    u
  }

  def possibleUniverses(predicates:Set[Predicate]):List[Universe] = allUniverses(
    extractEntities(predicates),
    extractRelations(predicates),
    extractBinaryRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def generateAtoms(e:Set[EntityConstant], r:Set[UnaryRelation]):Set[Predicate] = e flatMap {entity =>
    r flatMap {relation => List(Atom(relation, entity), Negation(Atom(relation, entity)))}}

  def generateBinaryAtoms(e:Set[EntityConstant], r:Set[BinaryRelation]):Set[Predicate] = e flatMap {e1 =>
    e flatMap {e2 =>
      r flatMap {relation => List(BinaryAtom(relation, e1, e2), Negation(BinaryAtom(relation, e1, e2)))}}}

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

  def generateExistentialConjunctions(from:Set[UnaryRelation]):Set[Predicate] = from flatMap {ra =>
    from flatMap {rb => ra match {
      case `rb` => None
      case _ => {
        val v = UniqueDesignations.variableDesignation(Translation.NoContext)
        val a = Atom(ra, v)
        val b = Atom(rb, v)
        List(Existential(v, Conjunction(a,b)),
          Existential(v, Conjunction(Negation(a), b)),
          Existential(v, Conjunction(a, Negation(b))),
          Existential(v, Conjunction(Negation(a), Negation(b))))
      }
    }}}

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
    val allConclusions = generateUniversalConditionals(relations) ++ generateAtoms(
      entities, relations) ++ generateExistentials(relations) ++ generateUniversals(relations) ++
      generateExistentialConjunctions(relations)
    validConclusions(allConclusions, universes)
  }

  def generateInterestingConclusions(priors:Set[Predicate]):Set[Predicate] = {
    val conclusions = generateConclusions(priors).filter {p => isInteresting(p, priors)}
    var uniqueConclusions = Set[Predicate]()
    conclusions.foreach{c => {
      if(uniqueConclusions.forall(d => c.equals(d) || !c.equivalent(d)))
        uniqueConclusions += c
    }}
    uniqueConclusions
  }


}

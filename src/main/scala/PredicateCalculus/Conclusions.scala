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
    //println("Finding all atomic universes...")
    val universes = Utils.refeed[List[List[Boolean]]](
      e.size * r.size, Utils.expandBitstring, List[List[Boolean]](List())) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i =>
          universe.relate(entity(i % e.size), relation(i / e.size), bitstring(i))
        }
        universe
      }
    //println("Finding all binary universes...")
    val binaryRelation = r2.toIndexedSeq
    //println("  all relations:" + binaryRelation)
    //println("  all entities:" + e)
    val binaryUniverses = Utils.refeed[List[List[Boolean]]](
      e.size * e.size * r2.size, Utils.expandBitstring, List[List[Boolean]](List())) map { bitstring =>
        val universe = new Universe()
        bitstring.indices.foreach { i => {
          universe.relate(entity(i % e.size), entity((i/e.size)%e.size),
            binaryRelation(i/(e.size*e.size)), bitstring(i))
          println(binaryRelation(i/(e.size*e.size)) + "(" + entity(i%e.size) + "," + entity((i/e.size)%e.size) + ")")
        }}
        universe
    }
    //println("Combining universes...")
    binaryUniverses flatMap{ bu => universes map { u => combineUniverses(List(bu, u)) }}
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

  def newPartialUniverses(rel:Set[UnaryRelation], rel2:Set[BinaryRelation],
      newEntities:Set[EntityConstant], oldEntities:Set[EntityConstant]):Set[Universe] = {
    //Atomic relations of new entities
    val e = newEntities.toIndexedSeq
    val r = rel.toIndexedSeq
    //println(e)
    //println(r)
    val newAtomUniverses:Set[Universe] = Set(new Universe()) ++
      (0 to 2^(e.size*r.size)).map{Utils.toBitstring(_, e.size*r.size)}.map{ bitstring:IndexedSeq[Boolean] =>
        val u = new Universe()
        bitstring.indices.foreach { i =>
          u.relate(e(i % e.size), r(i / e.size), bitstring(i))
        }
        u
      }.toSet
    //println(newAtomUniverses.size + " new atom universes.")
    //println("Atomic:" + newAtomUniverses)
    //Binary relations (new,old)
    val e_old = oldEntities.toIndexedSeq
    val r2 = rel2.toIndexedSeq
    val binarybitstrings = (0 to 2^(e_old.size*e.size*r2.size)).map{
        Utils.toBitstring(_, e.size*e_old.size*r2.size)}
    val newBinaryNewOldUniverses = Set(new Universe()) ++ binarybitstrings.map{ bitstring =>
        val u = new Universe()
        bitstring.indices.foreach { i =>
          u.relate(e(i % e.size), e_old((i/e.size)%e_old.size),
            r2(i/(e.size*e_old.size)), bitstring(i))
        }
        u
      }.toSet
    //println("Binary: " + newBinaryNewOldUniverses)
    //println(r2)
    //println(e_old.size)
    //Binary relations (old,new)
    val newBinaryOldNewUniverses = Set(new Universe()) ++ binarybitstrings.map{ bitstring =>
        val u = new Universe()
        bitstring.indices.foreach { i =>
          u.relate(e_old((i/e.size)%e_old.size), e(i % e.size),
            r2(i/(e.size*e_old.size)), bitstring(i))
        }
        u
      }.toSet
    val newBinaryNewNewUniverses = Set(new Universe()) ++ (0 to 2^(e.size*e.size*r2.size)).map(
          Utils.toBitstring(_, e.size*e.size*r2.size)).map{ bitstring =>
        val u = new Universe()
        bitstring.indices.foreach { i =>
          u.relate(e((i/e.size)%e.size), e(i % e.size),
            r2(i/(e.size*e.size)), bitstring(i))
        }
        u
      }.toSet
   // println(newBinaryNewNewUniverses.length * newBinaryOldNewUniverses.length *
    //  newBinaryNewOldUniverses.length * newAtomUniverses.length + " new universes")
    //println("Binary2: " + newBinaryOldNewUniverses)
    val i = newAtomUniverses.flatMap{a =>
      newBinaryNewOldUniverses.flatMap{b =>
        newBinaryOldNewUniverses.flatMap{c =>
          newBinaryNewNewUniverses.map{ d =>
            combineUniverses(List(a, b, c, d))
        }}}}
    //println(i)
    i
  }


  def possibleUniverses(predicates:Set[Predicate]):List[Universe] = allUniverses(
    extractEntities(predicates),
    extractRelations(predicates),
    extractBinaryRelations(predicates)).filter{u =>
      predicates.forall{p => p.evaluate(u)}
  }

  def possibleUniverses2(predicates:Set[Predicate]):List[Universe] = {
    val startUniverse = new Universe()
    startUniverse.unaryRelations ++= extractRelations(predicates)
    startUniverse.binaryRelations ++= extractBinaryRelations(predicates)
    var universes = Set[Universe](startUniverse)
    val r = extractRelations(predicates)
    val r2 = extractBinaryRelations(predicates)
    var pastPredicates = Set[Predicate]()
    //println(predicates)
    predicates foreach {predicate => {
      println(universes.size + " universes.")
      println("Incorporating predicate " + predicate + "...")
      val newEntities = predicate.entities
      //println(predicate)
      //println(newEntities)
      val oldEntities = pastPredicates.flatMap{_.entities}
      pastPredicates += predicate
      val npus = newPartialUniverses(r, r2, newEntities, oldEntities)
      universes = universes flatMap { universe => {
        //println("Universe " + universe)
        //val i = incorporate(universe, newEntities).filter{u => pastPredicates.forall{p => p.evaluate(u)}}
        //println("Incorporating " + i)
        val i = npus.map{u => combineUniverses(List(universe, u))}
        println(i.size + " new universes")
        val j = i.filter{u => pastPredicates.forall{p => p.evaluate(u)}}
        println(j.size + " after filtering")
        j
      }}
    }}
    universes.toList
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
    //println("Generating universes...")
    val universes:List[Universe] = possibleUniverses2(priors)
    if (universes.size == 0) return Set()
    val relations:Set[UnaryRelation] = universes.head.unaryRelations.toSet
    val entities:Set[EntityConstant] = universes.head.entities.toSet
    //println("Generating conclusions...")
    val allConclusions = generateUniversalConditionals(relations) ++ generateAtoms(
      entities, relations) ++ generateExistentials(relations) ++ generateUniversals(relations)
    //println("Filtering by validity...")
    validConclusions(allConclusions, universes)
  }

  def generateInterestingConclusions(priors:Set[Predicate]):Set[Predicate] =
    generateConclusions(priors).filter {p => isInteresting(p, priors)}


}

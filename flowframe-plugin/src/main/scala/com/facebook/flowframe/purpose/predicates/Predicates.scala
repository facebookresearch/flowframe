/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.facebook.flowframe.purpose.predicates

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

/** Predicates for Purpose Policies. */
trait Predicates {
    type ResolvantCache = mutable.Map[List[DisList], List[(Set[DisList], ClauseSet)]]

    def newResolvantCache(): ResolvantCache = {
        new mutable.WeakHashMap[List[DisList], List[(Set[DisList], ClauseSet)]]()
    }

    /** Predicates appear in ''with'' clauses. */
    trait Predicate

    /** Empty predicate; implicitly true */
    object True extends Predicate

    /** False object -- may be used as a NormalizedPred
     * (since CPreds cannot represent False) */
    object False extends Predicate with NormalizedPred

    /** Logical combinators */
    case class LNot(pred: Predicate) extends Predicate

    case class LAnd(pred1: Predicate, pred2: Predicate) extends Predicate

    case class LOr(pred1: Predicate, pred2: Predicate) extends Predicate

    /**
     * A comparison between two atoms: e.g., ''consent = true''
     *
     * @param a1
     * @param op
     * @param a2
     */
    case class BinOp(a1: Atom, op: Operation, a2: Atom) extends Predicate {
        override def toString: String =
            "(" + a1.toString + " " + op.toString + " " + a2.toString + ")"

        //  override def compare(that: BinOp): Int = {
        //    val a1cmp = a1.compare(that.a1)
        //    if (a1cmp == 0) {
        //      val a2cmp = a2.compare(that.a2)
        //      if (a2cmp == 0)
        //        op.compare(that.op)
        //      else
        //        a2cmp
        //    } else {
        //      a1cmp
        //    }
        //  }
    }

    trait Operation extends Ordered[Operation]

    // EQ < GT < GTE < LT < LTE
    case object EQ extends Operation {
        override def toString: String = "="

        override def compare(that: Operation): Int = {
            that match {
                case EQ => 0
                case _ => -1
            }
        }
    }

    case object GT extends Operation {
        override def toString: String = ">"

        override def compare(that: Operation): Int = {
            that match {
                case EQ => 1
                case GT => 0
                case _ => -1
            }
        }
    }

    case object GTE extends Operation {
        override def toString: String = ">="

        override def compare(that: Operation): Int = {
            that match {
                case EQ => 1
                case GT => 1
                case GTE => 0
                case _ => -1
            }
        }
    }

    case object LT extends Operation {
        override def toString: String = "<"

        override def compare(that: Operation): Int = {
            that match {
                case EQ => 1
                case GT => 1
                case GTE => 1
                case LT => 0
                case _ => -1
            }
        }
    }

    case object LTE extends Operation {
        override def toString: String = "<="

        override def compare(that: Operation): Int = {
            that match {
                case EQ => 1
                case GT => 1
                case GTE => 1
                case LT => 1
                case LTE => 0
                case _ => -1
            }
        }
    }

    /**
     * Atomic values that may appear in predicate comparisons
     */
    trait Atom extends Ordered[Atom]

    // Arbitrary ordering for normalization:
    // Variable < PurposeBool < PurposeInt < PurposeFloat < PurposeString

    /**
     * A runtime value, unknown at compile time. (e.g, a consent variable)
     *
     * @param variable
     */
    case class Variable(variable: String) extends Atom {
        override def toString: String = variable.toString

        override def compare(that: Atom): Int = that match {
            case thatVar: Variable => variable.compare(thatVar.variable)
            case _ => -1
        }
    }

    /**
     * Literal values
     */
    trait Literal extends Atom

    case class PurposeBool(bool: Boolean) extends Literal {
        override def toString: String = bool.toString

        override def compare(that: Atom): Int = that match {
            case _: Variable => 1
            case thatBool: PurposeBool => bool.compare(thatBool.bool)
            case _ => -1
        }
    }

    case class PurposeInt(n: Int) extends Literal {
        override def toString: String = n.toString

        override def compare(that: Atom): Int = that match {
            case _: Variable => 1
            case _: PurposeBool => 1
            case thatInt: PurposeInt => n.compare(thatInt.n)
            case _ => -1
        }
    }

    case class PurposeFloat(n: Float) extends Literal {
        override def toString: String = n.toString

        override def compare(that: Atom): Int = that match {
            case _: Variable => 1
            case _: PurposeBool => 1
            case _: PurposeInt => 1
            case thatFloat: PurposeFloat => n.compare(thatFloat.n)
            case _ => -1
        }
    }

    case class PurposeString(str: String) extends Literal {
        override def toString: String = str

        override def compare(that: Atom): Int = that match {
            case _: Variable => 1
            case _: PurposeBool => 1
            case _: PurposeInt => 1
            case _: PurposeFloat => 1
            case thatStr: PurposeString => str.compare(thatStr.str)
            case _ => -1
        }
    }

    /** Marker for normalized (CNF) predicates */
    trait NormalizedPred // user-friendly name

    val NormTrue = CPred(Set.empty)

    /** A CPred is a predicate in CNF form: a conjunction of disjunctions */
    case class CPred(ds: Set[DPred]) extends NormalizedPred {
        //override def compare(that: CPred): Int = ds.compare(that.ds)
    }

    /** A DPred is a disjunction of atomic predicates */
    case class DPred(as: Set[NPred]) {
        //override def compare(that: DPred): Int = as.compare(that.as)
    }

    case class NPred(binOp: BinOp, negated: Boolean) {
        override def toString: String =
            if (negated)
                "!" + binOp.toString
            else
                binOp.toString

        //override def compare(that: NPred): Int = {
        //  val bcmp = binOp.compare(that.binOp)
        //  if (bcmp == 0)
        //    negated.compare(that.negated)
        //  else
        //    bcmp
        //}
    }

    def simplifyPred(p: NormalizedPred): NormalizedPred = p //TODO: more simplification

    /**
     * Convert to negation normal form (NNF): move all negations inward.
     *
     * @param pred
     * @return pred in NNF
     */
    def normalizeNegation(pred: Predicate): Predicate = {
        pred match {
            case True => True
            case False => False
            case LNot(p) =>
                p match {
                    case True => False
                    case False => True
                    case LNot(q) => normalizeNegation(q)
                    case LAnd(q, r) =>
                        val nq = normalizeNegation(LNot(q)) match {
                            case True => return True
                            case False => return normalizeNegation(LNot(r))
                            case qq => qq
                        }
                        val nr = normalizeNegation(LNot(r)) match {
                            case True => return True
                            case False => return nq
                            case rr => rr
                        }
                        LOr(nq, nr)
                    case LOr(q, r) =>
                        val nq = normalizeNegation(LNot(q)) match {
                            case True => return normalizeNegation(LNot(r))
                            case False => return False
                            case qq => qq
                        }
                        val nr = normalizeNegation(LNot(r)) match {
                            case True => return nq
                            case False => return False
                            case rr => rr
                        }
                        LAnd(nq, nr)
                    case b: BinOp => LNot(b) // already normalized
                }
            case LAnd(p, q) =>
                val pp = normalizeNegation(p)
                val qq = normalizeNegation(q)
                if (pp == False || qq == False)
                    False
                else if (pp == True)
                    qq
                else if (qq == True)
                    pp
                else
                    LAnd(pp, qq)

            case LOr(p, q) =>
                val pp = normalizeNegation(p)
                val qq = normalizeNegation(q)
                if (pp == True || qq == True)
                    True
                else if (pp == False)
                    qq
                else if (qq == False)
                    pp
                else
                    LOr(pp, qq)
            case b: BinOp => b
        }
    }

    /**
     * Convert to CNF
     *
     * @param pred
     * @return NormalizedPred representing ''pred'' in CNF.
     *
     */
    def normalizePred(pred: Predicate): NormalizedPred = {
        normalizePred_(pred) match {
            case False => False
            //Sort terms to get a standard ordering
            case CPred(conjs) => CPred(conjs.map { case DPred(disjs) => DPred(disjs) })
        }
    }

    private def normalizePred_(pred: Predicate): NormalizedPred = {
        normalizeNegation(pred) match {
            case True => NormTrue
            case False => False
            case LOr(p, q) =>
                val pp@CPred(pconjuncts) = normalizePred_(p) match {
                    case False => return normalizePred_(q)
                    case pp => pp
                }
                val CPred(qconjuncts) = normalizePred_(q) match {
                    case False => return pp
                    case qq => qq
                }
                // compute the cross-product of the disjunctions of atoms
                val norm = CPred(
                    for {DPred(patoms) <- pconjuncts; DPred(qatoms) <- qconjuncts}
                        yield DPred(patoms ++ qatoms)
                )
                simplifyPred(norm)
            case LAnd(p, q) =>
                val pp@CPred(pconjuncts) = normalizePred_(p) match {
                    case False => return False
                    case pp => pp
                }
                val CPred(qconjuncts) = normalizePred_(q) match {
                    case False => return False
                    case qq => qq
                }
                val norm = CPred(pconjuncts ++ qconjuncts)
                simplifyPred(norm)
            case p: BinOp => binOpToNorm(p, false)
            case LNot(p@BinOp(_, _, _)) => binOpToNorm(p, true)
            case err => throw new RuntimeException(err.toString + " is not in normal negation form.")
        }
    }

    // TODO: consider converting congruent binOps to a single canonical form?
    def binOpToNorm(p: BinOp, negated: Boolean): NormalizedPred =
        CPred(Set(DPred(Set(NPred(p, negated)))))

    /** Does p imply q? */
    def implies(p: NormalizedPred, q: NormalizedPred)
               (implicit cache: ResolvantCache = newResolvantCache()): Boolean = {
        valid(normalizePred(LOr(convertPred(q), LNot(convertPred(p)))))
    }

    /** Is this predicate true for all variable assignments? */
    def valid(p: NormalizedPred)(implicit cache: ResolvantCache): Boolean = {
        val negated@CPred(conjs) = normalizePred(LNot(convertPred(p))) match {
            case False =>
                // negated predicate was false, therefore valid
                return true
            case NormTrue =>
                // negated predicate was true, therefore unsatisfiable
                return false
            case pp =>
                pp
        }
        val clauses: List[DisList] = (for {DPred(disjs) <- conjs} yield disjs).toList
        isUnsat(clauses) match {
            case Some(Step(parents, res, prev)) =>
                assert(parents != null)
                assert(res != null)
                assert(prev != null)
                true
            case _ => false
        }
    }

    /* Predicate resolution */

    type DisList = Set[NPred]
    type ClauseSet = Set[DisList]

    trait ProofStep

    case class Step(parents: ClauseSet, res: DisList, prev: ProofStep) extends ProofStep

    object Start extends ProofStep

    def isUnsat(clauses: List[DisList])(implicit cache: ResolvantCache): Option[ProofStep] = {
        val initOptions = findResolvants(clauses)
        val queue: Queue[ProofStep] = Queue.empty.enqueue(packResolvants(initOptions, Start).toList)
        isUnsat_(clauses, queue, Set.empty) match {
            case p@Some(_) => p
            case p@None => p
        }
    }

    @tailrec
    private def isUnsat_(baseClauses: List[DisList], queue: Queue[ProofStep],
                         seen: Set[ClauseSet])(implicit cache: ResolvantCache): Option[ProofStep] = {
        if (queue.isEmpty) return None
        val (current@Step(parents, res, prev), rest) = queue.dequeue
        if (res.isEmpty) return Some(current)
        val seen_ : Set[ClauseSet] = seen + parents
        val currentClauses: List[DisList] = res :: path(current)
        val neighbors = findResolvants(baseClauses ++ currentClauses)
        val newNeighbors = neighbors.filter { case (parents, _) => !seen_.contains(parents) }
        val queue_ : Queue[ProofStep] = rest.enqueue(packResolvants(newNeighbors, current))
        isUnsat_(baseClauses, queue_, seen_)
    }

    def convertPred(p: NormalizedPred): Predicate = {
        val CPred(conjs) = p match {
            case False => return False
            case pp => pp
        }
        (conjs.map { case DPred(disjs) =>
            ((disjs.map { case NPred(b, n) => if (n) LNot(b) else b }).toList match {
                case Nil => False
                case d :: ds => makePred(LOr, ds, d)
            })
        }).toList match {
            case Nil => True
            case d :: ds => makePred(LAnd, ds, d)
        }
    }

    def makePred(ctor: (Predicate, Predicate) => Predicate,
                 ps: List[Predicate], acc: Predicate): Predicate = {
        ps match {
            case Nil => acc
            case d :: ds => makePred(ctor, ds, ctor(acc, d))
        }
    }

    def congruents(binop: BinOp): Set[BinOp] = {
        binop match {
            case orig@BinOp(lhs, EQ, rhs) => Set(orig, BinOp(rhs, EQ, lhs))
            case orig@BinOp(lhs, GT, rhs) => Set(orig, BinOp(rhs, LT, lhs))
            case orig@BinOp(lhs, GTE, rhs) => Set(orig, BinOp(rhs, LTE, lhs))
            case orig@BinOp(lhs, LT, rhs) => Set(orig, BinOp(rhs, GT, lhs))
            case orig@BinOp(lhs, LTE, rhs) => Set(orig, BinOp(rhs, GTE, lhs))
            // BinOp? contains(ext_input___combination_policy_switches, 2)
        }
    }

    protected def findResolvants(clauses: List[DisList])(implicit cache: ResolvantCache): List[(Set[DisList], ClauseSet)] = {
        //check cache
        cache.get(clauses) match {
            case Some(res) => res
            case _ => val newRes: List[(Set[DisList], ClauseSet)] = {
                (for {List(d1, d2) <- clauses.combinations(2)}
                    yield Set(d1, d2) -> {
                        // the clause set resulting from resolving d1 and d2
                        val neighbor: ClauseSet = d1.filter {
                            // find all NPreds with a negated (congruent) cmp in another clause
                            case NPred(cmp, n) =>
                                congruents(cmp).exists(binOp => d2.contains(NPred(binOp, !n)))
                        }.map { case NPred(c, n) =>
                            // for each such NPred, return the list with the
                            // NPred and its negation removed
                            val d1res: Set[NPred] = congruents(c).map(binOp => NPred(binOp, n))
                            val d2res: Set[NPred] = congruents(c).map(binOp => NPred(binOp, !n))
                            d1.filter(!d1res.contains(_)).union(d2.filter(!d2res.contains(_)))
                        }
                        neighbor
                    }).toList
            }
                cache.put(clauses, newRes)
                newRes
        }
    }

    def packResolvants(xs: List[(Set[DisList], ClauseSet)], prev: ProofStep): List[ProofStep] = {
        xs.flatMap { case (set, rs) => rs.map(r => Step(set, r, prev)) }
    }

    def path(item: ProofStep): List[DisList] = {
        item match {
            case Start => List.empty[DisList]
            case Step(_, res: DisList, prev: ProofStep) => res :: path(prev)
        }
    }
}

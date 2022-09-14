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

package com.facebook.flowframe.purpose

import scala.collection.mutable
import com.facebook.flowframe.{AbstractPolicies, PolicyLang}
import com.facebook.flowframe.purpose.predicates.Predicates
import scala.collection.Map

import scala.tools.nsc.Global

sealed trait PurposePolicyLang extends PolicyLang

/** Purpose policy language. */
trait PurposePolicies extends AbstractPolicies[PurposePolicyLang] with Predicates {
    val global: Global

    /**
     * Purpose grammar
     * ------------
     * pol::= purpose
     * | any
     * | none
     * | pol and pol
     * | pol or pol
     * | pol with expr
     * //   * | not pol
     * pred::=
     * | pred and pred
     * | pred or pred
     * | not pred
     * | atom op atom
     * op :== > | >= | < | <= | =
     * //   * | create_set(expr,…)
     * //   * | contains(expr,expr)
     * //   * | ?configerator_ref(path,[string_literal,…])
     * atom ::= policy_variable
     * | string_literal
     * | integer_literal
     * | true
     * | false
     * //   * | is_set(policy_variable)
     */

    /**
     * Purpose Policies
     */
    implicit object lattice extends SecLattice {
        override val top: BasePolicy = Never
        override val bottom: BasePolicy = Always

        def join(p: PolicyExpr, q : PolicyExpr): PolicyExpr = And(p,q)

        def meet(p: PolicyExpr, q : PolicyExpr): PolicyExpr = Or(p,q)

        def simplify(p: PolicyExpr): PolicyExpr = convert(PurposePolicies.this.simplifyNorm(normalize(p))(this))

        def flowsto(lhs: PolicyExpr, rhs: PolicyExpr): Boolean = {
            PurposePolicies.this.flowstoNorm(normalize(lhs), normalize(rhs))(this)
        }
    }

    sealed trait PurposeExpr extends PolicyExpr {
        def join(p: PolicyExpr, q : PolicyExpr): PolicyExpr = And(p,q)

        override def unJoin: Option[(PolicyExpr, PolicyExpr)] = None

        def meet(p: PolicyExpr, q : PolicyExpr): PolicyExpr = Or(p,q)
        override def unMeet: Option[(PolicyExpr, PolicyExpr)] = None
    }

    /**
     * A specific
     *
     * @param parts
     */
    case class Purpose(parts: List[String]) extends BasePolicy with PurposeExpr {
        override def toString: String = parts.mkString(":")

        override def toBound: PolicyExpr = this

        override def flowsto(that: BasePolicy)
                            (implicit lattice: SecLattice): Boolean = {
            that match {
                case Never => true
                case Purpose(qs) => parts == qs ||
                    parts.slice(0, qs.length).corresponds(qs)(_ == _)
                case _ => false
            }
        }
    }

    /** Information may be used for *any purpose* */
    object Always extends BasePolicy with PurposeExpr {
        override def toString = "any"

        override def toBound: PolicyExpr = this

        override def flowsto(that: BasePolicy)(implicit lattice: SecLattice): Boolean = true
    }

    /** Information may that *may never be used* for any purpose */
    object Never extends BasePolicy with PurposeExpr {
        override def toString = "none"

        override def toBound: PolicyExpr = this
    }

    val NormNever: NormalizedPolicy = baseToNorm(Never)
    val NormAlways: NormalizedPolicy = baseToNorm(Always)

    case class Or(lhs: PolicyExpr, rhs: PolicyExpr) extends PurposeExpr {
        override def toString() =
            "(" + lhs.toString + ") or (" + rhs.toString + ")"

        override def toBound: PolicyExpr = Or(lhs.toBound, rhs.toBound)

        override def unMeet: Option[(PolicyExpr, PolicyExpr)] = Some((lhs,rhs))

        override def variables(includeUpperBounds: Boolean = true): Set[SolverVar] = {
            val lhsv = lhs.variables(includeUpperBounds)
            val rhsv = rhs.variables(includeUpperBounds)
            if (lhsv.isEmpty) rhsv
            else if (rhsv.isEmpty) lhsv
            else lhsv.union(rhsv)
        }

        override def parameters: Set[BoundedParameter] = {
            val lhsv = lhs.parameters
            val rhsv = rhs.parameters
            if (lhsv.isEmpty) rhsv
            else if (rhsv.isEmpty) lhsv
            else lhsv.union(rhsv)
        }

        override def subst(subst: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr])
                          (implicit lattice: SecLattice): PolicyExpr =
        {
            val lhsSubst = lhs.subst(subst, seen)
            val rhsSubst = rhs.subst(subst, seen)
            Or(lhsSubst, rhsSubst)
        }
    }

    case class And(lhs: PolicyExpr, rhs: PolicyExpr) extends PurposeExpr {
        override def toString() =
            "(" + lhs.toString + ") and (" + rhs.toString + ")"

        override def toBound: PolicyExpr = And(lhs.toBound, rhs.toBound)

        override def unJoin: Option[(PolicyExpr, PolicyExpr)] = Some((lhs,rhs))

        override def variables(includeUpperBounds: Boolean = true): Set[SolverVar] = {
            val lhsv = lhs.variables(includeUpperBounds)
            val rhsv = rhs.variables(includeUpperBounds)
            if (lhsv.isEmpty) rhsv
            else if (rhsv.isEmpty) lhsv
            else lhsv.union(rhsv)
        }

        override def parameters: Set[BoundedParameter] = {
            val lhsv = lhs.parameters
            val rhsv = rhs.parameters
            if (lhsv.isEmpty) rhsv
            else if (rhsv.isEmpty) lhsv
            else lhsv.union(rhsv)
        }

        override def subst(subst: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr])
                          (implicit lattice: SecLattice) : PolicyExpr =
        {
            val lhsSubst = lhs.subst(subst, seen)
            val rhsSubst = rhs.subst(subst, seen)
            And(lhsSubst, rhsSubst)
        }
    }

    case class With(policy: PolicyExpr, pred: Predicate) extends PurposeExpr {
        override def toString() = {
            if (pred == True)
                policy.toString
            else
                "(" + policy.toString + ") with (" + pred.toString + ")"
        }

        override def toBound: PolicyExpr = With(policy.toBound, pred)

        override def variables(includeUpperBounds: Boolean = true): Set[SolverVar] = policy.variables(includeUpperBounds)

        override def parameters: Set[BoundedParameter] = policy.parameters

        override def subst(subst: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr])
                          (implicit lattice: SecLattice) : PolicyExpr =
            With(policy.subst(subst, seen), pred)
    }

    case class Not(policy: PolicyExpr) extends PurposeExpr {
        override def toString() =
            "not (" + policy.toString + ")"

        override def toBound: PolicyExpr = Not(policy.toBound)

        override def variables(includeUpperBounds: Boolean = true): Set[SolverVar] =
            policy.variables(includeUpperBounds)

        override def parameters: Set[BoundedParameter] =
            policy.parameters

        override def subst(subst: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr])
                          (implicit lattice: SecLattice) : PolicyExpr =
            Not(policy.subst(subst, seen))
    }

    override def resolvePolicyExprRef(refContext: PolicyRefContext, pol: PolicyExpr): PolicyStruct = {
        pol match {
            // abstract policies
            case AbstractParam(_, _) =>
                PolicyStructExpr(pol)

            case argParam: ArgParam =>
                PolicyStructExpr(
                    new ArgParam(argParam.name, argParam.symbol, argParam.upperBound.map { bound =>
                        resolvePolicyExprRef(refContext, bound).asInstanceOf[PolicyStructExpr].expr
                    })
                )

            case selectParam: SelectParam =>
                PolicyStructExpr(
                    new SelectParam(selectParam.name, selectParam.upperBound.map { bound =>
                        resolvePolicyExprRef(refContext, bound).asInstanceOf[PolicyStructExpr].expr
                    })
                )

            case SolverVar(_, _, _) =>
                PolicyStructExpr(pol)

            // policy reference
            case PolicyExprRef(ref) => ref.resolvePolicyRef(refContext)

            // concrete policies
            case Never => PolicyStructExpr(Never)

            case Always => PolicyStructExpr(Always)

            case Purpose(_) => PolicyStructExpr(pol)

            case And(p, q) =>
                val p_resolved = resolvePolicyExprRef(refContext, p).asInstanceOf[PolicyStructExpr].expr
                val q_resolved = resolvePolicyExprRef(refContext, q).asInstanceOf[PolicyStructExpr].expr
                PolicyStructExpr(And(p_resolved, q_resolved))

            case Or(p, q) =>
                val p_resolved = resolvePolicyExprRef(refContext, p).asInstanceOf[PolicyStructExpr].expr
                val q_resolved = resolvePolicyExprRef(refContext, q).asInstanceOf[PolicyStructExpr].expr
                PolicyStructExpr(Or(p_resolved, q_resolved))

            case With(p, pred) =>
                val p_resolved = resolvePolicyExprRef(refContext, p).asInstanceOf[PolicyStructExpr].expr
                PolicyStructExpr(With(p_resolved, pred))
        }
    }

    /*
    def subst(subst: mutable.Map[AbstractPolicy, PolicyExpr], expr: PolicyExpr): PolicyExpr =
      expr.subst(subst)

    def substArgVars(subst: Map[BoundedParameter, PolicyExpr], expr: PolicyExpr): PolicyExpr = expr match {
      case v: BoundedParameter =>
        subst.get(v) match {
          case Some(bound) =>
            // only recurse if there are still parameters that can be replaced
            if (bound.parameters.filter(expr ne _).intersect(subst.keySet).isEmpty) bound
            else substArgVars(subst, bound)
          case None => v
        }
      case Or(p1, p2) => Or(substArgVars(subst, p1), substArgVars(subst, p2))
      case And(p1, p2) => And(substArgVars(subst, p1), substArgVars(subst, p2))
      case With(p1, p2) => With(substArgVars(subst, p1), p2)
      case e => e
    }
  */
    /**
     * Meet-Normal Form: represent policies as "meets of joins"
     */
    type NormalizedPolicy = MNorm // user-friendly name

    /** A MNorm is a meet (disjunction) of joins (conjunctions) */
    case class MNorm(js: Set[JNorm]) {
        def variables(): Set[SolverVar] = js.map(_.variables()).flatten

        def hasVariables(): Boolean = !variables().isEmpty
    }

    /** A JNorm is a join (conjunction) of with-constrained base policies */
    case class JNorm(ws: Set[WNorm]) {
        def variables(): Set[SolverVar] = ws.map(_.variables()).flatten

        def hasVariables(): Boolean = !variables().isEmpty
    }

    /** A WNorm is a with-constrained base policies */
    case class WNorm(policy: BasePolicy, pred: NormalizedPred) {
        def variables(includeUpperBounds: Boolean = true): Set[SolverVar] = policy match {
            case v: SolverVar => Set(v)
            case _ => Set.empty
        }

        def hasVariables(includeUpperBounds: Boolean = true): Boolean = !variables(includeUpperBounds).isEmpty
    }

    def baseToNorm(p: BasePolicy): NormalizedPolicy = {
        if (p == Never) {
            MNorm(Set.empty)
        } else {
            MNorm(Set(JNorm(Set(WNorm(p, NormTrue)))))
        }
    }


    def simplifyNorm(p: NormalizedPolicy)(implicit lattice: SecLattice): NormalizedPolicy = p match {
        case MNorm(js) => {
            val needed = new mutable.HashSet[JNorm]()
            for {j <- js.map(simplify(_))} {
                val notNeeded = new mutable.HashSet[JNorm]()
                if (j.hasVariables())
                    needed.add(j)
                else {
                    var subsumed = false
                    for {n <- needed} {
                        if (!n.hasVariables) {
                            if (flowstoNorm(n, j)) {
                                subsumed = true
                            } else if (flowstoNorm(j, n))
                                notNeeded.add(n) // new policy subsumes previously-needed policy
                        }
                    }
                    if (!subsumed) needed.add(j)
                    needed --= notNeeded
                }
            }
            MNorm(needed.toSet)
        }
    }

    def simplify(p: JNorm)(implicit lattice: SecLattice): JNorm = p match {
        case JNorm(ws) => {
            val needed = new mutable.HashSet[WNorm]()
            for {w <- ws} {
                val j = w.policy match {
                    case p: BoundedParameter if p.bound != p => WNorm(p.withNewBound(lattice.simplify(p.bound)), w.pred)
                    case _ => w
                }
                val notNeeded = new mutable.HashSet[WNorm]()
                if (j.hasVariables())
                    needed.add(j)
                else {
                    var subsumed = false
                    for {n <- needed} {
                        if (!n.hasVariables()) {
                            if (flowstoNorm(j, n)) {
                                subsumed = true
                            } else if (flowstoNorm(n, j))
                                notNeeded.add(n) // new policy subsumes previously-needed policy
                        }
                    }
                    if (!subsumed) needed.add(j)
                    needed --= notNeeded
                }
            }
            JNorm(needed.toSet)
        }
    }

    /**
     * Convert to Meet-normal form
     *
     * @param pol
     * @return normalized policy.
     *
     */
    def normalize(pol: PolicyExpr): NormalizedPolicy = {
        pol match {
            case Never =>  baseToNorm(Never)
            case Always => baseToNorm(Always)
            case And(p, q) =>
                val MNorm(pjoins) = normalize(p)
                val MNorm(qjoins) = normalize(q)
                // compute the cross-product of the conjunctions of policies
                val norm = MNorm(for {JNorm(pws) <- pjoins; JNorm(qws) <- qjoins} yield JNorm(pws ++ qws))
                simplifyNorm(norm)
            case Or(p, q) =>
                val MNorm(pconjuncts) = normalize(p)
                val MNorm(qconjuncts) = normalize(q)
                val norm = MNorm(pconjuncts ++ qconjuncts)
                simplifyNorm(norm)
            case With(p, pred) =>
                // push predicates down to the invidual base policies
                val MNorm(pjoins) = normalize(p)
                val CPred(ds_norm) = normalizePred(pred) match {
                    case False => return NormNever
                    case pp => pp
                }
                MNorm(for {JNorm(ws) <- pjoins}
                    yield JNorm(for {WNorm(q, CPred(ds)) <- ws}
                        yield WNorm(q, CPred(ds ++ ds_norm))))
            case p: BasePolicy => baseToNorm(p)
        }
    }

    def convert(p: MNorm): PolicyExpr = {
        val MNorm(disjs) = p
        disjs.map { case JNorm(conjs) =>
            conjs.map {
                case WNorm(b, w) =>
                    if (w == True)
                        b
                    else
                        With(b, convertPred(w))
            }.toList match {
                case Nil => Always
                case c :: cs => makePolicy(And, cs, c)
            }
        }.toList match {
            case Nil => Never
            case d :: ds => makePolicy(Or, ds, d)
        }
    }

    def makePolicy(ctor: (PolicyExpr, PolicyExpr) => PolicyExpr,
                   ps: List[PolicyExpr], acc: PolicyExpr): PolicyExpr = {
        ps match {
            case Nil => acc
            case p :: ps => makePolicy(ctor, ps, ctor(acc, p))
        }
    }

    /**
     *
     * @param p
     * @param q
     * @return
     */
    def flowstoNorm(p: MNorm, q: MNorm)(implicit lattice: SecLattice): Boolean = {
        if (p == NormAlways || q == NormNever) return true
        var b = false
        for {dq <- q.js} { // for each disjunct on the RHS,
            for {dp <- p.js} // find a disjunct on the LHS that flows to it
                b |= flowstoNorm(dp, dq) // does dp flow to dq?
            if (!b) return false // false => nothing on LHS flows to dq (insecure)
        }
        true
    }

    // TODO: implicit cache args?
    def flowstoNorm(p: JNorm, q: JNorm)(implicit lattice: SecLattice): Boolean = {
        val JNorm(pws) = p
        val JNorm(qws) = q

        var b = false
        for {pw <- pws} { // for each conjunct on the LHS,
            for {qw <- qws} // find a conjunct on the RHS that it flows to
                b |= flowstoNorm(pw, qw)
            if (!b) return false
        }
        true
    }

    def flowstoNorm(p: WNorm, q: WNorm)(implicit lattice: SecLattice): Boolean = {
        val WNorm(pb, ppred) = p
        val WNorm(qb, qpred) = q
        pb.flowsto(qb) && implies(qpred, ppred)
    }
}

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

package com.facebook.flowframe

import scala.collection.mutable
import scala.reflect.api.Position
import scala.reflect.internal.util.NoPosition
import scala.collection.Map

trait PolicyLang

trait Policies[T <: PolicyLang] {
    trait SecLattice {
        val top: BasePolicy
        val bottom: BasePolicy

        def join(p: PolicyExpr, q: PolicyExpr): PolicyExpr

        def meet(p: PolicyExpr, q: PolicyExpr): PolicyExpr

        def simplify(p: PolicyExpr): PolicyExpr

        def flowsto(lhs: PolicyExpr, rhs: PolicyExpr): Boolean
    }

    /** Policy expression */
    trait PolicyExpr {
        def join(p: PolicyExpr)(implicit lattice: SecLattice): PolicyExpr =
            lattice.simplify(lattice.join(this, p))

        def unJoin: Option[(PolicyExpr, PolicyExpr)]

        def meet(p: PolicyExpr)(implicit lattice: SecLattice): PolicyExpr = {
            lattice.simplify(lattice.meet(this, p))
        }

        def unMeet: Option[(PolicyExpr, PolicyExpr)]

        /** [[SolverVar]]s in the expression. */
        def variables(includeUpperBounds: Boolean = true): Set[SolverVar] = Set.empty

        def hasVariables(includeUpperBounds: Boolean = true): Boolean =
            variables(includeUpperBounds).nonEmpty

        /** Bounded parameters in the expression. */
        def parameters: Set[BoundedParameter] = Set.empty

        /** Subtitute expressions as given by a map. */
        def subst(substMap: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr] = Set.empty)
                 (implicit lattice: SecLattice): PolicyExpr = this

        /** Replaces [[BoundedParameter]] expressions with their bounds. */
        def toBound: PolicyExpr
    }

    /**
     * BasePolicies are policies that describe a single atomic policy
     */
    trait BasePolicy extends PolicyExpr {
        def flowsto(that: BasePolicy)(implicit lattice: SecLattice): Boolean = {
            that == this || that == lattice.top
        }

        override def unJoin: Option[(PolicyExpr, PolicyExpr)] = None

        override def unMeet: Option[(PolicyExpr, PolicyExpr)] = None
    }

    // TODO?: reconcile subst implementations used by SolverVar and ArgParams

    /** An AbstractPolicy represents a policy that can be instantiated with a (eg concrete) policy. */
    trait AbstractPolicy extends BasePolicy {
        def name: String

        override def subst(substMap: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr])
                          (implicit lattice: SecLattice): PolicyExpr = {
            // if a substitute exists, replace this abstract policy, otherwise just return it
            // NB: this try/catch form is important to support substMaps with default values.  Do not use getOrElse.
            try {
                substMap(this)
            } catch {
                case _: NoSuchElementException => this
            }
        }

        // TODO: need to refactor to avoid name collisions
        override def toString: String = s"{$name}"
    }

    trait BoundedParameter extends AbstractPolicy {
        def bound: PolicyExpr

        def cycleOp(implicit lattice: SecLattice): (PolicyExpr, PolicyExpr) => PolicyExpr

        def withNewBound(policy: PolicyExpr): BoundedParameter

        override def variables(includeUpperBounds: Boolean = true): Set[SolverVar] =
            if (bound != this && includeUpperBounds)
                bound.variables(includeUpperBounds)
            else
                super.variables(includeUpperBounds)

        override def toBound: PolicyExpr = this.bound

        override def parameters: Set[BoundedParameter] = Set(this)

        override def subst(substMap: Map[AbstractPolicy, PolicyExpr], seen: Set[PolicyExpr] = Set.empty)
                          (implicit lattice: SecLattice): PolicyExpr = {
            substMap.get(this) match {
                // case Some(sub) if seen.contains(sub) =>
                //     // if there is a cycle in the bound graph, then apply cycleOp to the cycle. Typically this is a meet or join
                //     // 1) all policies in the cycle are equivalent, and 2) for GLB, this parameter flows to anything
                //     // we can prove at least one of these policies flows to
                //     lattice.simplify(seen.foldLeft(sub)((acc, p) => cycleOp(lattice)(acc, p)))

                case Some(sub) =>
                    // only recurse if we haven't seen this bound before
                    sub.subst(substMap, seen + sub)

                // even if no substitutions for this bounded parameter, there might be substs for its bound
                case None if this.bound != this => withNewBound(this.bound.subst(substMap))

                case None => this
            }
        }

        def findUpperBound(policy: PolicyExpr, seen: Set[PolicyExpr] = Set.empty)
                          (implicit lattice: SecLattice): PolicyExpr = {
            val ubMap = new mutable.LinkedHashMap[AbstractPolicy, PolicyExpr]
            for {param <- policy.parameters} {
                if (param.bound ne param)
                    ubMap.put(param, param.bound)
            }
            policy.subst(ubMap.asInstanceOf[Map[AbstractPolicy,PolicyExpr]], seen)
        }

        override def flowsto(that: BasePolicy)(implicit lattice: SecLattice): Boolean = {
            if (super.flowsto(that)) true
            else {
                val ub = findUpperBound(this, Set(this))
                if (ub != this) lattice.flowsto(ub, that)
                else false
            }
        }
    }

    protected[flowframe] case class SolverVar(name: String, uid: Int, pos: Position = NoPosition)
        extends AbstractPolicy {

        override def toBound: PolicyExpr = this

        override def toString: String = s"$name:${uid.toString}"

        override def variables(includeUpperBounds: Boolean = true): Set[SolverVar] = Set(this)
    }
}

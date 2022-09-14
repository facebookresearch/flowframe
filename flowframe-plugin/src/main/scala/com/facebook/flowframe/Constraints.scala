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

trait Constraints[T <: PolicyLang] extends Policies[T] {
    /** Policy constraint. */
    trait Constraint {
        /** Is the constraint satisfied? */
        def isSatisfied: Boolean

        /** Left-hand side of the expression. */
        def lhs: PolicyExpr

        /** Right-hand side of the expression. */
        def rhs: PolicyExpr

        /** Source file position associated with the constraint. */
        def pos: Position

        /** Debugging information. */
        def debug: PolicyViolationDebugInfo

        /** Substitute abstract policies with concrete policies. */
        def subst(subst: mutable.Map[AbstractPolicy, PolicyExpr])
                 (implicit lattice: SecLattice): Constraint = {
            FlowsToConstraint(lhs.subst(subst, Set.empty), rhs.subst(subst, Set.empty), pos, debug=debug)
        }
    }

    /** Constraint that states the left-hand side should be at most as restrictive as the right-hand side. */
    case class FlowsToConstraint(lhs: PolicyExpr, rhs: PolicyExpr, pos:Position,
                                 trace:Array[StackTraceElement] = new Exception().getStackTrace, debug: PolicyViolationDebugInfo)
                                (implicit lattice: SecLattice) extends Constraint {
        override def isSatisfied: Boolean = {
            lattice.flowsto(lhs, rhs)
        }

        override def toString: String = s"{$lhs <= $rhs}"
    }
}

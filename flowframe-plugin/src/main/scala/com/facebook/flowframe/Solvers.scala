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

import scala.collection.{mutable => m}
import scala.reflect.api.Position
import scala.reflect.internal.util.NoPosition

/** Policy constraint solver.
 *  Core algorithm is based on Jif's GLB solver: https://www.cs.cornell.edu/jif/
 * */
trait Solvers[T <: PolicyLang] extends Constraints[T] {
   type ConstraintQueue = m.Queue[Constraint]

    object Status extends Enumeration {
        type Status = Value
        val NOT_SOLVED, SOLVING, SOLVED, NO_SOLUTION = Value
    }

    private var variable_counter: Int = 0

    private def nextUID(): Int = {
        val next = variable_counter
        variable_counter += 1
        next
    }

    def freshSolverVariable(name: String, pos: Position = NoPosition): SolverVar = {
        SolverVar(name, nextUID(), pos)
    }

    // TODO: isn't this a LUBSolver?
    class GLBSolver(lattice: SecLattice) {

        import Status._

        /**
         * Return the most permissive policy P such that lhs <= (rhs join P)
         */
        def findNeeded(lhs: PolicyExpr, rhs: PolicyExpr)(implicit lattice: SecLattice): PolicyExpr = {
            lhs.unJoin match {
                case Some((p1, p2)) =>
                    val maybeNeeded1: Option[PolicyExpr] =
                        if (!FlowsToConstraint(p1, rhs, NoPosition, debug = UnknownPolicyViolation).isSatisfied)
                            Some(findNeeded(p1, rhs))
                        else
                            None
                    val maybeNeeded2: Option[PolicyExpr] =
                        if (!FlowsToConstraint(p2, rhs, NoPosition, debug = UnknownPolicyViolation).isSatisfied)
                            Some(findNeeded(p2, rhs))
                        else
                            None

                    (maybeNeeded1, maybeNeeded2) match {
                        case (Some(q1), Some(q2)) => lattice.join(q1, q2)
                        case (Some(q1), _) => q1
                        case (_, Some(q2)) => q2
                        case _ => throw new Error("something weird happened")
                    }
                case None =>
                    lhs.unMeet match {
                        case Some((p1, p2)) => lattice.meet(findNeeded(p1, rhs), findNeeded(p2, rhs))
                        case _ => lhs
                    }
            }
            // TODO: figure out correct handling of other types
            //      case v:SolverVar => v
            //      case Not(policy) => ???
            //      case With(p1, p2) => inner(p1) ++ inner(p2)
            //      case Compare(p1, op, p2) => inner(p1) ++ inner(p2)
            //      case Contains(value, container) => inner(value) ++ inner(container)
            //      case CreateSet(set) =>
            //        set.fold(LinkedHashSet.newBuilder[SolverVar])(op = _ ++ inner((_: PolicyExpr)))
            //      case _ => LinkedHashSet.newBuilder[SolverVar]
        }

        def allMultiVarsOnRHS(Q: ConstraintQueue): Boolean = {
            for (c <- Q) {
                if (c.rhs.variables().size <= 1)
                    return false
            }
            true
        }

        /** Queue of active constraints */
        protected var Q = new ConstraintQueue

        /** All constraints */
        protected var constraint_set: m.Set[Constraint] = new m.LinkedHashSet[Constraint]

        /** Failed constraints */
        protected var failedConstraints = new m.MutableList[Constraint]

        /**
         * Map from variables to (Set of) equations that may be invalidated by
         * the variable changing. That is, if the bound of
         * the variable v changes, then we may need to re-examine all equations in
         * (Set)varEqnDependencies.get(v)
         */
        protected var varEqnDependencies: m.Map[SolverVar, m.Set[Constraint]] =
            new m.HashMap[SolverVar, m.Set[Constraint]]

        /**
         * Map from variables to (Set of) equations that may be invalidated by
         * the variable changing. That is, if the bound of
         * the variable v changes, then we may need to re-examine all equations in
         * (Set)varEqnDependencies.get(v)
         */
        protected var eqnVarDependencies: m.Map[Constraint, m.Set[SolverVar]] =
            new m.HashMap[Constraint, m.Set[SolverVar]]

        /**
         * Map from variables to (Set of) equations that may change the value
         * of the variable. That is, when satisfying any equation in
         * (Set)varEqnReverseDependencies.get(v), the value of v may be changed.
         */
        protected var varEqnReverseDependencies: m.Map[SolverVar, m.Set[Constraint]] =
            new m.HashMap[SolverVar, m.Set[Constraint]]

        /**
         * Map from equations to (Set of) variables in which a change in value
         * may invalidate this equation. That is, when the value of any variable
         * in (Set)eqnVarReverseDependencies.get(e), changes, the
         * equation e may be invalidated.
         */
        protected var eqnVarReverseDependencies: m.Map[Constraint, m.Set[SolverVar]] =
            new m.HashMap[Constraint, m.Set[SolverVar]]

        /**
         * Set of Variables that had their initial value fixed when the constraint
         * was added.
         */
        protected var fixedValueVars: m.Set[SolverVar] =
            new m.HashSet[SolverVar]

        def isFixedValueVar(v: SolverVar) = fixedValueVars.contains(v)

        /**
         * Constraints that were added to the solver, and failed statically.
         * If the flag THROW_STATIC_FAILED_CONSTRAINTS is true, then the
         * constraint will be thrown immediately, otherwise the constraint
         * will be added to this set, and thrown when solve() is called.
         */
        protected var staticFailedConstraints: m.Set[Constraint] = new m.HashSet[Constraint]

        protected var status = NOT_SOLVED

        protected def defaultBound(implicit lattice: SecLattice): PolicyExpr = lattice.bottom
        protected var varBounds : m.Map[AbstractPolicy, PolicyExpr] =
            new m.HashMap[AbstractPolicy, PolicyExpr].withDefaultValue(defaultBound(lattice))

        def constraintSet: m.Set[Constraint] = constraint_set

        /**
         * Perform a mostly-shallow copy.
         *
         * @return A shallow copy of this solver with deep copies of [[varBounds]], [[Q]], and [[constraint_set]],
         *         and an empty [[failedConstraints]]
         */
        final def copy(implicit lattice: SecLattice): GLBSolver = {
            val copy = new GLBSolver(lattice)
            copy.Q = new ConstraintQueue() ++= Q
            copy.constraint_set = constraint_set.to[m.LinkedHashSet]
            copy.varEqnDependencies = varEqnDependencies
            copy.eqnVarDependencies = eqnVarDependencies
            //copy.traces
            copy.status = status
            copy.fixedValueVars = fixedValueVars
            copy.failedConstraints = new m.MutableList[Constraint]
            copy.varBounds = new m.HashMap[AbstractPolicy, PolicyExpr].withDefaultValue(defaultBound) ++= varBounds
            copy
        }

        def setBound(v: SolverVar, newBound: PolicyExpr, responsible: Constraint) = {
            assert(newBound != null)
            varBounds.put(v, newBound)
        }

        def addConstraint(c: Constraint): Unit = {
            if (c.lhs == c.rhs) return
            constraint_set.add(c)
            addDependencies(c)
        }

        protected def addConstraintToQueue(c: Constraint): Unit = {
            if (!Q.contains(c)) Q.enqueue(c)
        }

        protected def addConstraintToQueueHead(c: Constraint): Unit = {
            Q.+:(c)
        }

        protected def addDependencies(c: Constraint): Unit = {
            val changeable: Set[SolverVar] = c.rhs.variables()
            val awakeable: Set[SolverVar] = c.lhs.variables()
            // If this constraint is examined, then the bound for v may be changed
            for {v <- changeable} addDependency(c, v)
            // If the bound for v is changed (upward), then we may need to
            // reexamine this equation.
            for {v <- awakeable} addDependency(v, c)
        }

        protected def addDependency(c: Constraint, v: SolverVar) {
            val cs: m.Set[Constraint] =
                if (!varEqnReverseDependencies.contains(v)) {
                    val cs = new m.LinkedHashSet[Constraint]
                    varEqnReverseDependencies.put(v, cs)
                    cs
                } else {
                    varEqnReverseDependencies(v)
                }
            cs.add(c)

            val vs: m.Set[SolverVar] =
                if (!eqnVarDependencies.contains(c)) {
                    val vs = new m.LinkedHashSet[SolverVar]
                    eqnVarDependencies.put(c, vs)
                    vs
                } else {
                    eqnVarDependencies(c)
                }
            vs.add(v)
        }

        protected def addDependency(v: SolverVar, c: Constraint) {
            val cs: m.Set[Constraint] =
                if (!varEqnDependencies.contains(v)) {
                    val cs = new m.LinkedHashSet[Constraint]
                    varEqnDependencies.put(v, cs)
                    cs
                } else {
                    varEqnDependencies(v)
                }
            cs.add(c)

            val vs: m.Set[SolverVar] =
                if (!eqnVarReverseDependencies.contains(c)) {
                    val vs = new m.LinkedHashSet[SolverVar]
                    eqnVarReverseDependencies.put(c, vs)
                    vs
                } else {
                    eqnVarReverseDependencies(c)
                }
            vs.add(v)
        }

        /**
         * Solve the system of constraints. If the system has already been solved,
         * then returned the cached solution.
         *
         * @throws Exception if the Solver cannot find a solution to the
         *                   system of contraints.
         */
        def solve(implicit lattice: SecLattice): m.Map[AbstractPolicy, PolicyExpr] = {
            if (status == SOLVED || status == NO_SOLUTION)
                return varBounds

            assert(status != SOLVING)
            status = SOLVING

            if (staticFailedConstraints.nonEmpty) {
                status = NO_SOLUTION
                throw new Error("Failed static constraints! TODO: better error msg")
            }

            for {c <- constraint_set}
                if (!Q.contains(c)) Q.enqueue(c)
            assert(Q.size == constraint_set.size)
            try {
                val solution = solveBounds
                status = SOLVED
                solution
            } catch {
                case se: Exception => //TODO: custom exception class?
                    status = NO_SOLUTION
                    throw se
            }
        }

        /**
         * Solve the system of constraints by finding upper bounds for the label
         * variables.
         *
         * @return a solution to the system of constraints, in the form of a VarMap
         *         of the upper bounds of the label variables.
         * @throws Exception if the Solver cannot find a solution to the
         *                   system of constraints.
         */
        def solveBounds(implicit lattice: SecLattice): m.Map[AbstractPolicy, PolicyExpr] = {
            while (!Q.isEmpty) {
                considerConstraint(Q.dequeue())
            }
            checkCandidateSolution
            varBounds
        }

        def checkCandidateSolution(implicit lattice: SecLattice): Unit = {
            for (c: Constraint <- constraint_set) {
                // transformations for robustness checking like
                // writersToReaders would occur here
                val csub = c.subst(varBounds)
                if (!csub.isSatisfied)
                    throw new InternalError("not satisfied")
            }
        }

        /**
         * Awakens all constraints in the system that depend on the variable v,
         * ensuring that they are in the queue of active constraints.
         */
        protected def wakeUp(v: SolverVar): Unit = {
            val ocs = varEqnDependencies.get(v)
            ocs match {
                // Add to ConstraintQueue (unless already present)
                case Some(cs) =>
                    for {c <- cs} {
                        if (!Q.contains(c)) Q.enqueue(c)
                    }
                case None => return
            }
        }

        def refineVariableConstraint(v: SolverVar, c: Constraint, bool: Boolean)
                                    (implicit lattice: SecLattice): Unit = {
            val varBound: PolicyExpr = varBounds(v)
            val csub = c.subst(varBounds)
            val needed = findNeeded(csub.lhs, csub.rhs)
            val newBound = lattice.join(varBound, needed)
            setBound(v, newBound, c)
            wakeUp(v)
        }

        /**
         * Search recursively for solution to system of constraints.
         */
        protected def search(c: Constraint)(implicit lattice: SecLattice): Boolean = {
            val newSolver = copy
            // make sure this equation is satisfied before continuing.
            newSolver.addConstraintToQueueHead(c)
            // try solving -- returns solution or throws exception
            varBounds = newSolver.solveBounds
            return true
        }

        /** Try to solve a single constraint. */
        def considerConstraint(c: Constraint)(implicit lattice: SecLattice): Unit = {
            val csub = c.subst(varBounds)
            if (!csub.isSatisfied) {
                if (!c.rhs.hasVariables(false)) {
                    val relevantVarBounds = c.lhs.variables().map(v => (v, lattice.simplify(varBounds(v)))).toMap
                    throw PolicyConstraintViolationException(c, relevantVarBounds)
                }

                // rhs has at least one non-upperbound variable.
                val rhsVars = c.rhs.variables(false)
                val isSingleVar = rhsVars.size == 1
                val oneVar = rhsVars.head
                if (isSingleVar && !isFixedValueVar(oneVar)) {
                    // only a single component is a variable
                    refineVariableConstraint(oneVar, c, true)
                    return
                } else if (!isSingleVar && !allMultiVarsOnRHS(Q)) {
                    // some of the active constraints have single variables
                    // on the RHS. Satisfy those first, to reduce the search
                    // effort.
                    addConstraintToQueue(c)
                    return
                } else {
                    val originalBounds = new m.HashMap[AbstractPolicy, PolicyExpr].withDefaultValue(defaultBound)
                    originalBounds ++= varBounds
                    var lastexception: Exception = null
                    var lastlabel: PolicyExpr = null
                    var lastvar: SolverVar = null
                    for (v <- rhsVars) {
                        // if this var label had its value fixed when its constraint
                        // was added. Do not try to alter it's value.
                        if (!isFixedValueVar(v)) {
                            refineVariableConstraint(oneVar, c, false)
                            try {
                                if (c.subst(varBounds).isSatisfied && search(c)) {
                                    //success!!
                                    return
                                }
                            } catch {
                                case se: Exception => //TODO: custom exception class?
                                    // solution failed! need to backtrack
                                    lastexception = se
                                    lastlabel = varBounds(v)
                                    lastvar = v
                            }
                        }
                        varBounds = originalBounds
                    }
                    // out of variables to try, search failed
                    if (lastexception != null) {
                        // addtrace
                        throw lastexception
                    } else {
                        throw new Exception("Search failed") // TODO: custom exception
                    }
                }
            } else {
                //satisfied
            }
        }
    }

    /** Exception thrown when a policy constraint fails to hold.
     *  @param c         The constraint that failed
     *  @param varBounds The values given by the solver to variables in the constraint at the time of failure
     * */
    final case class PolicyConstraintViolationException(c: Constraint, varBounds: Map[SolverVar, PolicyExpr]) extends Exception {
        def generateViolationMessage(): String = {
            val builder = new m.StringBuilder()
            val lines: Array[String] = c.pos.source.content.mkString.split('\n')

            val beginLine = 1.max(c.pos.line - PolicyConstraintViolationException.NUM_CONTEXT_LINES)
            val endLine = lines.length.min(c.pos.line + PolicyConstraintViolationException.NUM_CONTEXT_LINES)

            builder.append(s"${c.debug.shortName}\n")
            builder.append(s"${c.debug.description}\n\n")
            builder.append(s"${c.pos.source.path}\n")

            // print highlighted error line, along with some context
            for (lineno <- Range(beginLine, endLine+1, 1)) {
                val linestr = lines.apply(lineno-1)
                if (lineno == c.pos.line) {
                    builder.append(f">>>> $lineno%5d $linestr\n")
                } else {
                    builder.append(f"     $lineno%5d $linestr\n")
                }
            }

            builder.append('\n')
            builder.append("Constraint:\n")
            builder.append(s"${c.toString}")
            builder.append('\n')
            builder.append("Substitutions:\n")
            for ((k: AbstractPolicy, v: PolicyExpr) <- varBounds) {
                builder.append(s"$k = $v\n")
            }
            builder.append('\n')
            builder.toString()
        }
    }

    object PolicyConstraintViolationException {
        val NUM_CONTEXT_LINES = 3
    }
}

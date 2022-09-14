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
import scala.tools.nsc.Global

trait AbstractPolicies[T <: PolicyLang] extends Solvers[T] {
    val global: Global
    import global._

    /**
     * A policy variable denoting an external policy that the compiler should reason about abstractly.
     */
    case class AbstractParam(name: String, symbol: Symbol) extends AbstractPolicy {
        override def toBound: PolicyExpr = this
    }


    /** An abstract variable denoting the policy of a (parametric) function parameter
     * that is instantiated at the call site. */
    // NB: currently `symbol` is excluded from equality comparisons, but it may be necessary to revisit
    //     this if we support annotation syntax for ArgParams and SelectParams.  Subclassing complicates
    //     disambiguating BoundedParameters, esp SelectParams.
    class ArgParam(val name: String, val symbol: Symbol, val upperBound: Option[PolicyExpr] = None) extends BoundedParameter {
        private val _upperBound: PolicyExpr = upperBound.getOrElse(this)

        override def bound: PolicyExpr = _upperBound

        // TODO: need to refactor to avoid name collisions
        // override def hashCode(): RunId = name.hashCode + upperBound.hashCode()
        override def hashCode(): Int = name.hashCode

        override def equals(that: Any): Boolean = {
            that match {
                case thatAV: ArgParam =>
                    this.name == thatAV.name

                case _ => false
            }
        }

        /*
        override def equals(that: Any): Boolean = {
            super.equals(that) || (if (that.isInstanceOf[ArgParam]) {
                val thatAV = that.asInstanceOf[ArgParam]
                name == thatAV.name
                /*
                name == thatAV.name && // symbol == thatAV.symbol && // XXX: see note above
                    (if (_upperBound ne this) _upperBound == thatAV._upperBound
                    else thatAV._upperBound eq thatAV // different symbol *object* but otherwise equivalent
                        )
                 */
            } else {
                false
            })
        }
        */

        override def toString: String = {
            val upperBoundStr = if (_upperBound != this) s" <= ${_upperBound.toString}" else ""
            s"arg($name$upperBoundStr)"
        }

        override def parameters: Set[BoundedParameter] = Set(this)

        /**
         * If a cycle is found in the upper-bound graph, treat the meet (Or) of
         * the policies in the cycle as the upper bound since
         * 1) all policies in the cycle are equivalent, and
         * 2) this parameter flows to anything we can prove at least one of these
         * policies flows to
         */
        override def cycleOp(implicit lattice: SecLattice): (PolicyExpr, PolicyExpr) => PolicyExpr =
            lattice.meet

        def withNewBound(bound: PolicyExpr): ArgParam =
            new ArgParam(name, symbol, Some(bound))
    }

    /** An abstract variable denoting the policy of the access path to the dataset
     * that is instantiated at the call site. */
    class SelectParam(val name: String, val upperBound: Option[PolicyExpr] = None) extends BoundedParameter {
        override def hashCode(): Int = this.name.hashCode

        override def equals(that: Any): Boolean = {
            that match {
                case thatAV: SelectParam =>
                    this.name == thatAV.name

                case _ => false
            }
        }

        private val _upperBound: PolicyExpr = upperBound.getOrElse(this)

        override def bound: PolicyExpr = _upperBound

        override def toString: String = {
             "select(" + name + (if (_upperBound != this) " <= " + _upperBound else "") + ")"
        }

        override def flowsto(that: BasePolicy)(implicit lattice: SecLattice): Boolean =
            super.flowsto(that) ||
                (that match {
                    case that: SelectParam => true
                    //symbol.tpe == that.symbol.tpe // TODO: revisit!
                    case _ => false
                })

        /**
         * If a cycle is found in the upper-bound graph, treat the meet (Or) of
         * the policies in the cycle as the upper bound since
         * 1) all policies in the cycle are equivalent, and
         * 2) this parameter flows to anything we can prove at least one of these
         * policies flows to
         */
        override def cycleOp(implicit lattice: SecLattice): (PolicyExpr, PolicyExpr) => PolicyExpr =
            lattice.meet

        override def withNewBound(bound: PolicyExpr): SelectParam = {
            new SelectParam(name, Some(bound))
        }
    }

    /** The context in which a policy reference will be resolved.
     *  This information is computed at the call sites of label-polymorphic functions. */
    case class PolicyRefContext(accessPath: PolicyExpr, arguments: Map[Name, PolicyStruct])

    final case class UnresolvedPolicyRefException(name: String)
        extends Exception(s"Cannot resolve policy reference $name")

    /** Reference to another policy. */
    abstract sealed class PolicyRef() {
        /** Resolve policy references to the policies they point to.
         *
         * @param ctx the context in which to resolve the reference.
         * @return the referent policy expression
         * */
        def resolvePolicyRef(ctx: PolicyRefContext)(implicit lattice: SecLattice): PolicyStruct
    }

    /** Reference to the current access path label. */
    object ReceiverRef extends PolicyRef {
        override def toString: String = "this"

        override def resolvePolicyRef(ctx: PolicyRefContext)(implicit lattice: SecLattice): PolicyStruct = {
            PolicyStructExpr(ctx.accessPath)
        }
    }

    /** Reference to an argument's policy. */
    case class ArgPolicyRef(arg: Name) extends PolicyRef {
        override def toString: String = arg.toString

        override def resolvePolicyRef(ctx: PolicyRefContext)(implicit lattice: SecLattice): PolicyStruct = {
            ctx.arguments.get(arg) match {
                case Some(argTree) => argTree
                case None => throw UnresolvedPolicyRefException(this.toString)
            }
        }
    }

    /** Reference to an argument's policy signature. */
    case class ArgPolicySigRef(arg: Name, refType: PolicySigRefType) extends PolicyRef {
        override def toString: String = s"${arg.toString}.${refType.name}"

        override def resolvePolicyRef(ctx: PolicyRefContext)(implicit lattice: SecLattice): PolicyStruct = {
            ctx.arguments.get(arg) match {
                case Some(argTree) =>
                    val argSig = argTree.asInstanceOf[PolicySignature]
                    refType match {
                        case PolicySigReturnRef => argSig.returnPolicy
                    }

                case None => throw UnresolvedPolicyRefException(this.toString)
            }
        }
    }

    /** Type of reference to a policy signature. */
    sealed abstract class PolicySigRefType {
        def name: String
    }

    /** Reference to a policy signature's return type. */
    object PolicySigReturnRef extends PolicySigRefType {
        override def name: String = "ret"
    }

    /** A policy that refers to another policy.
     * This is used for methods with polymorphic labels. */
    case class PolicyExprRef(ref: PolicyRef) extends AbstractPolicy {
        override def name: String = s"ref($ref)"

        override def toBound: PolicyExpr = this
    }

    /** Policies for structural types like functions and tuples. */
    sealed abstract class PolicyStruct {
        /** Instantiate abstract policies within the struct with concrete policy expressions. */
        def subst(map: mutable.Map[AbstractPolicy, PolicyExpr])(implicit lattice: SecLattice): PolicyStruct

        /** Resolve policy references within the policy struct. */
        def resolvePolicyRef(ctx: PolicyRefContext): PolicyStruct

        /** Check if another policy struct has the same "shape" as this one. */
        def compatibleWith(other: PolicyStruct): Boolean

        /** Increase restriction on  policy structure's access path policy . */
        def joinAccessPath(accessPath: PolicyExpr)(implicit lattice: SecLattice): PolicyStruct

        /** Join with a compatible policy struct. */
        def join(other: PolicyStruct)(implicit lattice: SecLattice): PolicyStruct

        /** Meet with a compatible policy struct. */
        def meet(other: PolicyStruct)(implicit lattice: SecLattice): PolicyStruct
    }

    /** Policy structure for functions/methods. */
    final case class PolicySignature(accessPath: PolicyExpr, beginPolicy: BoundedParameter, paramssPolicies: List[List[PolicyStruct]], returnPolicy: PolicyStruct)
        extends PolicyStruct {

        override def toString: String = {
            val paramssStr = paramssPolicies.map { params => s"(${params.map(_.toString).mkString(",")})" }.mkString
            s"{$beginPolicy}$paramssStr => $returnPolicy"
        }

        override def subst(map: mutable.Map[AbstractPolicy, PolicyExpr])(implicit lattice: SecLattice): PolicyStruct = {
            PolicySignature(
                accessPath = this.accessPath.subst(map),
                beginPolicy = this.beginPolicy.withNewBound(this.beginPolicy.subst(map)),
                paramssPolicies = this.paramssPolicies.map { params => params.map { param => param.subst(map) } },
                returnPolicy = this.returnPolicy.subst(map)
            )
        }

        override def resolvePolicyRef(ctx: PolicyRefContext): PolicyStruct = {
            PolicySignature(
                accessPath = this.accessPath,
                beginPolicy = this.beginPolicy,
                paramssPolicies = this.paramssPolicies.map { paramPols =>
                    paramPols.map { paramPol => paramPol.resolvePolicyRef(ctx) }
                },
                returnPolicy = this.returnPolicy.resolvePolicyRef(ctx)
            )
        }

        override def compatibleWith(other: PolicyStruct): Boolean = {
            other match {
                case PolicySignature(otherAccessPath, otherBegin, otherParamss, otherReturn) =>
                    this.returnPolicy.compatibleWith(otherReturn) &&
                        this.paramssPolicies.length == otherParamss.length &&
                        this.paramssPolicies.zip(otherParamss).forall { case (params, otherParams) =>
                            params.zip(otherParams).forall { case (param, otherParam) => param.compatibleWith(otherParam) }
                        }

                case _ => false
            }
        }

        override def joinAccessPath(otherAccessPath: PolicyExpr)(implicit lattice: SecLattice): PolicySignature = {
            this.copy(accessPath = this.accessPath.join(otherAccessPath))
        }

        override def join(other: PolicyStruct)(implicit lattice: SecLattice): PolicyStruct = {
            other match {
                case PolicyStructExpr(_) =>
                    throw PolicyStructMismatchException(this, other)

                case PolicySignature(otherAccessPath, otherBegin, otherParamss, otherReturn) if this.compatibleWith(other) =>
                    // access path is covariant
                    val joinAccessPath = this.accessPath.join(otherAccessPath)

                    /** begin labels are contravariant in subtyping, so joined signature should
                     * have _meet_ of begin labels */
                    val joinBegin = this.beginPolicy.meet(otherBegin)

                    /** like begin labels, argument labels are contravariant in subtyping, so joined signature should
                     *  have the _meet_ of argument labels */
                    val joinParamss = this.paramssPolicies.zip(otherParamss).map { case (thisParams, otherParams) =>
                        thisParams.zip(otherParams).map { case (thisParam, otherParam) =>
                            thisParam.meet(otherParam)
                        }
                    }

                    /** return label is covariant, so join signature should have the join of return policies  */
                    val joinReturn = this.returnPolicy.join(otherReturn)

                    PolicySignature(
                        accessPath = joinAccessPath,
                        beginPolicy = joinBegin.asInstanceOf[BoundedParameter],
                        paramssPolicies = joinParamss,
                        returnPolicy = joinReturn
                    )
            }
        }

        override def meet(other: PolicyStruct)(implicit lattice: SecLattice): PolicyStruct = {
            other match {
                case PolicyStructExpr(_) =>
                    throw PolicyStructMismatchException(this, other)

                case PolicySignature(otherAccessPath, otherBegin, otherParamss, otherReturn) if this.compatibleWith(other) =>
                    // access path is covariant
                    val meetAccessPath = this.accessPath.meet(otherAccessPath)

                    /** begin labels are contravariant in subtyping, so meet signature should
                     * have _join_ of begin labels */
                    val meetBegin = this.beginPolicy.join(otherBegin)

                    /** like begin labels, argument labels are contravariant in subtyping, so meet signature should
                     *  have the _join_ of argument labels */
                    val meetParamss = this.paramssPolicies.zip(otherParamss).map { case (thisParams, otherParams) =>
                        thisParams.zip(otherParams).map { case (thisParam, otherParam) =>
                            thisParam.join(otherParam)
                        }
                    }

                    /** return label is covariant, so meet signature should have the meet of return policies  */
                    val meetReturn = this.returnPolicy.meet(otherReturn)

                    PolicySignature(
                        accessPath = meetAccessPath,
                        beginPolicy = meetBegin.asInstanceOf[BoundedParameter],
                        paramssPolicies = meetParamss,
                        returnPolicy = meetReturn
                    )
            }
        }
    }

    /** Policy struct wrapper around policy expressions. */
    case class PolicyStructExpr(expr: PolicyExpr) extends PolicyStruct {
        override def toString: String = expr.toString

        override def subst(map: mutable.Map[AbstractPolicy, PolicyExpr])(implicit lattice: SecLattice): PolicyStruct = {
            val substExpr = this.expr.subst(map)
            PolicyStructExpr(substExpr)
        }

        override def resolvePolicyRef(ctx: PolicyRefContext): PolicyStruct = resolvePolicyExprRef(ctx, expr)

        override def compatibleWith(other: PolicyStruct): Boolean = other.isInstanceOf[PolicyStructExpr]

        override def joinAccessPath(accessPath: PolicyExpr)(implicit lattice: SecLattice): PolicyStruct = {
            PolicyStructExpr(this.expr.join(accessPath))
        }

        override def join(other: PolicyStruct)(implicit lattice: SecLattice): PolicyStruct = {
            other match {
                case PolicyStructExpr(otherExpr) => PolicyStructExpr(this.expr.join(otherExpr))
                case PolicySignature(_, _, _, _) => throw PolicyStructMismatchException(this, other)
            }
        }

        override def meet(other: PolicyStruct)(implicit lattice: SecLattice): PolicyStruct = {
            other match {
                case PolicyStructExpr(otherExpr) => PolicyStructExpr(this.expr.meet(otherExpr))
                case PolicySignature(_, _, _, _) => throw PolicyStructMismatchException(this, other)
            }
        }
    }

    final case class PolicyStructMismatchException(s1: PolicyStruct, s2: PolicyStruct)
        extends Exception(s"Policy struct mismatch between $s1 and $s2")

    /** Resolve policy references in a policy expression.
     *  This changes depending on the policy language, so must be implemented by subclasses. */
    def resolvePolicyExprRef(ctx: PolicyRefContext, expr: PolicyExpr): PolicyStruct

    /** Extract policy struct from annotation. */
    def getPolicyStruct(anns: List[AnnotationInfo]): Option[PolicyStruct]
}

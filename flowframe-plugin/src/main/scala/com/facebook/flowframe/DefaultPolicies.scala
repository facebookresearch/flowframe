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

import scala.tools.nsc.Global

trait DefaultPolicies[T <: PolicyLang] extends AbstractPolicies[T] {
    val global: Global
    import global._

    def nameForParam(param: Symbol): String = {
        param.owner.fullNameString + "." + param.name
    }

    /** Generate a default policy signature for a symbol. */
    def defaultPolicySignature(sym: Symbol)(implicit lattice: SecLattice): PolicySignature = {
        val isCaseApply = sym.isCaseApplyOrUnapply && sym.nameString == "apply"

        val classPol: Option[PolicyExpr] =
            if (isCaseApply) {
                getPolicyStruct(sym.tpe.resultType.typeSymbol)
                    .flatMap { pol => Some(pol.asInstanceOf[PolicyStructExpr].expr) }

            } else {
                getPolicyStruct(sym.enclClass)
                    .flatMap { pol => Some(pol.asInstanceOf[PolicyStructExpr].expr) }
            }

        val accessPath: PolicyExpr = lattice.bottom

        // For primary constructors and copy methods, the class policy should be a begin label.
        // This means that control flow (PC) and access path policies invoking these methods should not exceed
        // the class policy.
        val beginPolicy =
        if ((sym.isPrimaryConstructor || sym.isCaseCopy || isCaseApply) && classPol.isDefined) {
            new SelectParam(sym.enclClass.fullNameString, classPol)

        } else {
            val varName = sym.fullNameString + "$begin"
            val beginFreshVar = freshSolverVariable(varName).asInstanceOf[PolicyExpr]
            new SelectParam(varName, Some(beginFreshVar))
        }

        // for primary constructors and copy methods,
        // policy on enclosing class should lower bound parameter policies since they are fields
        val paramssPolicies: List[List[PolicyStruct]] =
        if ((sym.isPrimaryConstructor || sym.isCaseCopy || isCaseApply) && classPol.isDefined) {
            sym.paramss.map { params =>
                params.map { param =>
                    // for now, assume all constructor params are not functions
                    val polBound: PolicyExpr =
                        getPolicyStruct(param, withDefault=false, persist=false)
                            .getOrElse(PolicyStructExpr(freshSolverVariable(nameForParam(param))))
                            .asInstanceOf[PolicyStructExpr].expr
                            .join(classPol.get)

                    PolicyStructExpr(new ArgParam(nameForParam(param), param, upperBound = Some(polBound)))
                }
            }
        } else {
            sym.paramss.map { params =>
                params.map { param =>
                    // assume parameter is not a function unless specified otherwise
                    getPolicyStruct(param, withDefault=false, persist=false).getOrElse {
                        val solverVar = freshSolverVariable(nameForParam(param), param.pos)
                        PolicyStructExpr(new ArgParam(s"${solverVar.name}:${solverVar.uid}", param, upperBound = Some(solverVar)))
                    }
                }
            }
        }

        val returnPolicy = {
            // for constructors and constructor-like methods, return the class policy
            // if no class policy exists, assume that the return policy of the constructor is
            // the join of the access path and the argument policies, as is the default in regular functions
            if ((sym.isPrimaryConstructor || sym.isCaseCopy || isCaseApply) && classPol.isDefined) {
                PolicyStructExpr(classPol.get)

                // TODO Rolph: infer the right policy struct for methods and lambdas instead of just assuming policy struct expr
                // the default return policy is the join of the begin and param policies
            } else {
                // joined begin and param policies is the default return policy
                val joinedBeginAndParams: PolicyExpr =
                    paramssPolicies.flatten.foldLeft(lattice.bottom.asInstanceOf[PolicyExpr]) { (acc, param) =>
                        param match {
                            case PolicyStructExpr(paramPolicy) => acc.join(paramPolicy)
                            case PolicySignature(sigAccessPath, _, _, _) => acc.join(sigAccessPath)
                        }
                    }.join(beginPolicy)

                PolicyStructExpr(joinedBeginAndParams)
            }
        }

        PolicySignature(accessPath, beginPolicy, paramssPolicies, returnPolicy)
    }

    /** Default policy is a solver variable. */
    def defaultPolicyStructExpr(name: String, pos: Position): PolicyStructExpr = {
        val solverVar = freshSolverVariable(name, pos)
        PolicyStructExpr(solverVar)
    }

    def defaultPolicyStruct(sym: Symbol)(implicit lattice: SecLattice): PolicyStruct = {
        // TODO Rolph also check that return type is not a function before converting to policy struct expr
        // TODO Rolph handle Function types and anonymous functions?
        if (sym.isMethod && sym.paramss.nonEmpty) {
            defaultPolicySignature(sym)

        } else {
            defaultPolicyStructExpr(sym.fullNameString, sym.pos)
        }
    }

    def getPolicyStruct(node: Tree): Option[PolicyStruct] = {
        node.attachments.get[PolicyStruct]
    }

    /** Fetch the policy struct for a symbol. This function tries the following, in order:
     * - fetch existing symbol policy struct
     * - fetch it from an annotation
     * - compute a default policy struct for the symbol
     * */
    def getPolicyStruct(sym: Symbol, withDefault: Boolean = false, persist: Boolean = true)(implicit lattice: SecLattice): Option[PolicyStruct] = {
        if (sym.hasAttachment[PolicyStruct]) {
            Some(sym.attachments.get[PolicyStruct].get)

        } else {
            val annPol = getPolicyStruct(sym.annotations)
            val typeAnnPol = getPolicyStruct(sym.tpe.annotations)
            val resTypeAnnPol = getPolicyStruct(sym.tpe.resultType.annotations)

            val newPol: Option[PolicyStruct] =
                // fetch annotation from the symbol directly
                if (annPol.isDefined) {
                    annPol

                    // fetch annotation from symbol's type
                } else if (typeAnnPol.isDefined) {
                    typeAnnPol

                    // if symbol is an accessor, fetch annotation from the result type
                } else if (sym.isAccessor && resTypeAnnPol.isDefined) {
                    resTypeAnnPol

                    // fetch default policy
                } else if (withDefault) {
                    Some(defaultPolicyStruct(sym))

                } else {
                    None
                }

            // TODO: class policy must lower bound all member policies?
            /*
            val newPolWithClass = newPol.flatMap { pol =>
                if (sym.enclClass != sym & sym.enclClass != NoSymbol) {
                    getPolicyStruct(sym.enclClass, withDefault = false).flatMap{ classPol =>
                        val classPolExpr = classPol.asInstanceOf[PolicyStructExpr].expr
                        Some(pol.joinAccessPath(classPolExpr))
                    }.orElse(Some(pol))
                } else Some(pol)
            }
            */

            // cache the policy as an attachment
            if (newPol.isDefined && persist) {
                sym.updateAttachment[PolicyStruct](newPol.get)

                // persist policy signature to parameter symbols also
                newPol.get match {
                    case sig: PolicySignature =>
                        sym.paramss.zip(sig.paramssPolicies).foreach { case (params, policies) =>
                            params.zip(policies).foreach { case (param, policy) =>
                                param.updateAttachment[PolicyStruct](policy)
                            }
                        }

                    case _ =>
                }
            }

            newPol
        }
    }
}

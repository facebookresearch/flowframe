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
import scala.reflect.internal.util.{SourceFile, Statistics}
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}

/** Compiler phase to check information flow policies. */
abstract class CheckLabels[T <: PolicyLang] extends PluginComponent with SparkSignatures[T] {
    val global: Global
    import global._

    val labelcheckNanos: Statistics.Timer = Statistics.newTimer("time spent labelchecking", "check-labels")
    val phaseName: String = "check-labels"

    override def description = "Check Purpose Policies Phase"

    override def newPhase(prev: Phase) : LabelCheckPhase = new LabelCheckPhase(prev)

    protected var enabled_flag = false

    override def enabled: Boolean = enabled_flag

    def enable(): Unit =
        enabled_flag = true

    def disable(): Unit =
        enabled_flag = false

    lazy protected val FlowframeShouldPassAnnotation = rootMirror.getRequiredClass("com.facebook.flowframe.FlowframeShouldPass")
    lazy protected val FlowframeShouldFailAnnotation = rootMirror.getRequiredClass("com.facebook.flowframe.FlowframeShouldFail")

    protected val unitsShouldPass: mutable.Map[SourceFile, String] = mutable.Map()
    protected val unitsShouldFail: mutable.Map[SourceFile, String] = mutable.Map()

    // units that have been processed by FlowFrame already
    protected val processedUnits: mutable.Set[CompilationUnit] = mutable.Set()

    /** The phase defined by this transform. */
    class LabelCheckPhase(prev: Phase) extends StdPhase(prev) {
        override def run(): Unit = {
            try {
                val start = if (Statistics.canEnable) Statistics.startTimer(labelcheckNanos) else null
                global.echoPhaseSummary(this)
                    for (unit <- currentRun.units) {
                        applyPhase(unit)
                        undoLog.clear()
                    }
                if (Statistics.canEnable) Statistics.stopTimer(labelcheckNanos, start)
                super.run()
            } catch {
                case exn: PolicyConstraintViolationException =>
                    global.reporter.echo(s"Flowframe has failed on ${exn.c.pos.source.path}")
                    global.reporter.echo(exn.generateViolationMessage())
            }
        }

        /** Check policies for a single compilation unit */
        override def apply(unit: CompilationUnit): Unit = {
            try {
                // only processed non-synthetic compilation units that have not been processed before
                if (!unit.body.symbol.isSynthetic && !processedUnits.contains(unit)) {
                    processedUnits.add(unit)
                    val solver = new GLBSolver(lattice)
                    val lc: LabelChecker = LabelChecker(solver, lattice.bottom, lattice.bottom, lattice.bottom)
                    lc.traverse(unit.body)
                    if (global.settings.Yrangepos && !global.reporter.hasErrors) global.validatePositions(unit.body)
                    for (workItem <- unit.toCheck) workItem()

                    val solution = lc.solver.solve(lattice)

                    // if we reach here, this means the unit was successful
                    // by default, if there is no annotation assume that the unit was supposed to succeed
                    if (!unitsShouldFail.contains(unit.source)) {
                        val passDesc = unitsShouldPass.get(unit.source)

                        if (passDesc.isDefined){
                            global.reporter.echo(s"${unit.source.path} has succeeded FlowFrame pass as expected: ${passDesc.get}")

                        } else {
                            global.reporter.echo(s"${unit.source.path} has succeeded FlowFrame pass as expected")
                        }

                    } else { // unit passed but was expected to fail
                        val failDesc = unitsShouldFail(unit.source)
                        global.reporter.echo(s"${unit.source.path} has succeeded FlowFrame pass but was expected to fail: $failDesc")
                    }

                    // print debugging info

                    /*
                    global.reporter.echo(s"Source ${unit.source.file.name}")
                    global.reporter.echo(unit.body.toString())

                    global.reporter.echo("Constraints")
                    for (constr <- lc.solver.constraintSet) {
                        global.reporter.echo(s"${constr.toString} info: ${constr.debug.shortName} pos: ${constr.pos}")
                    }

                    global.reporter.echo("Apply solution")
                    for ((k, v) <- solution) {
                        var vsub = v
                        while (vsub.hasVariables())
                            vsub = vsub.subst(solution, Set(v))(lattice)
                        global.reporter.echo(unit.source + ":: Solution: Inferred bounds for variable " + k.toString + " = " + lattice.simplify(vsub))
                    }
                    */
                }

            } catch {
                case c: PolicyConstraintViolationException =>
                    if (unitsShouldFail.contains(unit.source)) {
                        val desc = unitsShouldFail(unit.source)
                        global.reporter.echo(s"${unit.source.path} has failed FlowFrame pass as expected: $desc")

                    } else { // if failure is unexpected, re-throw the exception to halt compilation
                        throw c
                    }

            } finally {
                unit.toCheck.clear()
            }
        }
    }

    /** Traverses a compilation unit to generate policy constraints. */
    case class LabelChecker(solver: GLBSolver, pc: PolicyExpr, codeSelectBound: PolicyExpr, ambientAccessPath: PolicyExpr) extends Traverser {
        val specialSyms: Set[Symbol] = CheckLabels.this.sparkSignatures

        /** Check that the labels in a policy structure is a subtype of another policy structure.
         *  This generates a series of flows-to constraints.
         *
         *  @param s1 The struct that must be a subtype.
         *  @param s2 The struct that must be a supertype.
         * */
        def policyStructFlowsTo(s1: PolicyStruct, s2: PolicyStruct, pos: Position, debug: PolicyViolationDebugInfo): Unit = {
            (s1, s2) match {
                case (PolicyStructExpr(expr1), PolicyStructExpr(expr2)) => {
                    // when generating constraints, replace BoundedParameters with their bounds
                    // this maintains the invariant that constraints are only over base policy expressions and solver variables
                    solver.addConstraint(FlowsToConstraint(expr1.toBound, expr2.toBound, pos = pos, debug = debug))
                }

                /* This case must satisfy function subtyping: this means that return policies are _covariant_,
                * while begin and argument policies are _contravariant_.
                * This means that if sig1 is to be a subtype of sig2, then sig2's begin policy and arguments policies
                * must flow to sig1's respective begin policy and argument policies, and sig1's return policy must
                * flow to sig2's return policy.
                *
                * See the "Method Declaration" section in the Jif manual
                * (https://www.cs.cornell.edu/jif/doc/jif-3.3.0/manual.html).
                */
                case (PolicySignature(accessPath1, begin1, paramss1, return1), PolicySignature(accessPath2, begin2, paramss2, return2))
                    if s1.compatibleWith(s2) =>

                    // access path policy is covariant
                    // TODO Rolph figure out if this should generate constraints for accessPath labels also
                    // solver.addConstraint(FlowsToConstraint(accessPath1, accessPath2, pos = pos, debug = debug))

                    // begin policies must be contravariant
                    solver.addConstraint(FlowsToConstraint(begin2.bound, begin1.bound, pos = pos, debug = debug))

                    // parameter policies must be contravariant
                    paramss1.zip(paramss2).foreach { case (params1, params2) =>
                        params1.zip(params2).foreach { case (param1, param2) =>
                            policyStructFlowsTo(param2, param1, pos = pos, debug = debug)
                        }
                    }

                    // return policies must be covariant
                    policyStructFlowsTo(return1, return2, pos = pos, debug = debug)

                case _ =>
                    throw PolicyStructMismatchException(s1, s2)
            }
        }

        /** Collect argument policy substitutions for bounded parameters (ArgParams and SelectParams) at callsites.
         *
         *  @param substMap Substitution map
         *  @param param    Parameter's policy struct
         *  @param arg      Argument's policy struct
         * */
        def collectArgumentPolicySubstitutions(substMap: mutable.Map[AbstractPolicy, PolicyExpr], param: PolicyStruct, arg: PolicyStruct): Unit = {
            (param, arg) match {
                case _ if !param.compatibleWith(arg) =>
                    throw PolicyStructMismatchException(param, arg)

                case (PolicyStructExpr(paramExpr), PolicyStructExpr(argExpr)) =>
                    if (paramExpr.isInstanceOf[ArgParam]) {
                        substMap.put(paramExpr.asInstanceOf[ArgParam], argExpr)
                    }

                case (paramSig: PolicySignature, argSig: PolicySignature) =>
                    if (paramSig.beginPolicy.isInstanceOf[SelectParam]) {
                        substMap.put(paramSig.beginPolicy.asInstanceOf[SelectParam], argSig.beginPolicy)
                    }

                    paramSig.paramssPolicies.zip(argSig.paramssPolicies).foreach { case (paramsPolicies, argsPolicies) =>
                        paramsPolicies.zip(argsPolicies).foreach { case (paramParam, argArg) =>
                            collectArgumentPolicySubstitutions(substMap, paramParam, argArg)
                        }
                    }

                    collectArgumentPolicySubstitutions(substMap, paramSig.returnPolicy, argSig.returnPolicy)
            }
        }

        /** Policy checking of (value application) call sites. */
        def handleCallsite(callsite: Tree, fun: Tree, args: List[Tree]): Unit = {
            val unresolvedFuncSig: PolicySignature = getPolicyStruct(fun).get.asInstanceOf[PolicySignature]
            val accessPath = unresolvedFuncSig.accessPath.join(pc)
            val argPolicies: List[PolicyStruct] = args.map(getPolicyStruct(_).get.joinAccessPath(accessPath))

            // resolve policy references
            val refContext = PolicyRefContext(
                accessPath = accessPath,
                arguments = fun.tpe.params.map(_.name).zip(argPolicies).toMap
            )

            val funcSig = unresolvedFuncSig.resolvePolicyRef(refContext).asInstanceOf[PolicySignature]
            val substMap: mutable.Map[AbstractPolicy, PolicyExpr] = new mutable.LinkedHashMap[AbstractPolicy, PolicyExpr]()

            val paramPolicies: List[PolicyStruct] = for {param <- fun.tpe.params} yield {
                fun.symbol.paramss.flatten.find(_.name == param.name) match {
                    case Some(p) => getPolicyStruct(p, withDefault=true).get
                    case None =>
                        throw new InternalError(s"Could not lookup parameter symbol ${param.name} in function ${fun.symbol.name}")
                }
            }

            // access path and pc at callsite must flow to begin policy upperbound
            substMap.put(funcSig.beginPolicy, accessPath)

            if (args.length != fun.tpe.params.length && !definitions.isRepeatedParamType(fun.tpe.params.last.tpe)) {
                throw new InternalError("Failed sanity check: # of args and # of params don't match!")
            }

            // populate substitution map with argument policies
            for {(paramPol, argPol) <- paramPolicies zip argPolicies} {
                collectArgumentPolicySubstitutions(substMap, paramPol, argPol)
            }

            // instantiate return policy; make sure it is lower bounded by the access path
            // return policy must be lower bounded by access path!
            val returnPolicy = funcSig.returnPolicy.subst(substMap).joinAccessPath(accessPath)

            // is this the result a partially-applied function or the final result?
            if (fun.tpe.resultType != fun.tpe.finalResultType) {
                // attach a partially-instantiated policy signature to the Apply AST node
                val newBegin = funcSig.beginPolicy.withNewBound(funcSig.beginPolicy.bound.subst(substMap))
                val newParamss: List[List[PolicyStruct]] =
                    funcSig.paramssPolicies.map { paramPols =>
                        paramPols.map { paramPol => paramPol.subst(substMap) }
                    }.tail

                callsite.updateAttachment[PolicyStruct](PolicySignature(accessPath, newBegin, newParamss, returnPolicy))

            } else {
                callsite.updateAttachment[PolicyStruct](returnPolicy)
            }

            // ensure the access path flows to the function's begin policy
            policyStructFlowsTo(
                PolicyStructExpr(accessPath),
                PolicyStructExpr(funcSig.beginPolicy),
                pos = callsite.pos,
                debug = BeginPolicyViolation
            )

            // ensure that the arguments flow to the parameter policies
            for {((arg, argPol), paramPol) <- args.zip(argPolicies).zip(paramPolicies)} {
                // val instPol = paramPol.subst(substMap)
                policyStructFlowsTo(argPol, paramPol, pos = arg.pos, debug = ParameterPolicyViolation)
            }
        }

        /** Traverse AST and generate policy constraints. */
        override def traverse(tree: Tree): Unit = {
            tree match {
                // use special FlowframeTest module to tag whether a compilation unit should fail a Flowframe test or not
                case t@ModuleDef(mods, name, impl) => {
                    for {ann <- t.symbol.annotations.filter(_.matches(FlowframeShouldFailAnnotation))
                         (n, arg) <- ann.javaArgs}
                    yield {
                        if (n.toString.equals("value")) {
                            arg match {
                                case LiteralAnnotArg(const) =>
                                    unitsShouldFail.put(t.pos.source, const.stringValue)

                                case _ => throw SyntaxError("Unexpected argument type for FlowframeShouldFail")
                            }
                        }
                    }

                    for {ann <- t.symbol.annotations.filter(_.matches(FlowframeShouldPassAnnotation))
                         (n, arg) <- ann.javaArgs}
                    yield {
                        if (n.toString.equals("value")) {
                            arg match {
                                case LiteralAnnotArg(const) =>
                                    unitsShouldPass.put(t.pos.source, const.stringValue)

                                case _ => throw SyntaxError("Unexpected argument type for FlowframeShouldPass")
                            }
                        }
                    }

                    traverse(impl)
                }

                // don't traverse synthetically generated classes
                case t@ClassDef(modifiers, name, defs, template) if t.symbol.isSynthetic => ()

                case t@ValDef(mods, name, tpt, rhs) =>
                    super.traverse(tree)  // visit children

                    if (!t.hasAttachment[PolicyStruct]) {
                        // assume that the definition annotation overrides the type annotation
                        // if neither exist, resort to a default policy (generate a solver variable)
                        val defAnnOpt = getPolicyStruct(t.symbol, withDefault=false)
                        val tptAnnOpt = getPolicyStruct(t.tpt.symbol, withDefault=false)
                        val pol = defAnnOpt.orElse(tptAnnOpt).getOrElse { defaultPolicyStruct(t.symbol) }
                        // val pol = defAnnOpt.getOrElse { defaultPolicyStruct(t.symbol) }

                        t.symbol.updateAttachment[PolicyStruct](pol)
                        t.updateAttachment[PolicyStruct](pol)

                        // if there is an assignment value, make sure its policy flows to the valdef's declared policy
                        if (!rhs.isEmpty) {
                            val rhsPol = getPolicyStruct(rhs).get
                            policyStructFlowsTo(
                                rhsPol.joinAccessPath(pc).joinAccessPath(ambientAccessPath),
                                pol,
                                pos = rhs.pos,
                                debug = ValDefRHSPolicyViolation
                            )
                        }
                    }

                case t@Literal(_) =>
                    if (!t.hasAttachment[PolicyStruct]) {
                        // literals don't leak information, and thus always have the least restrictive policy ("public")
                        t.updateAttachment[PolicyStruct](PolicyStructExpr(lattice.bottom))
                    }

                case t@Ident(name) =>
                    if (!t.hasAttachment[PolicyStruct]) {
                        val policy = getPolicyStruct(t.symbol, withDefault=true).get
                        t.updateAttachment[PolicyStruct](policy)
                    }

                case t@Assign(lhs, rhs) =>
                    // visit children
                    traverse(lhs)
                    traverse(rhs)

                    if (!t.hasAttachment[PolicyStruct]) {
                        val lhsPol = getPolicyStruct(lhs).get
                        val rhsPol = getPolicyStruct(rhs).get
                        policyStructFlowsTo(
                            rhsPol.joinAccessPath(pc).joinAccessPath(ambientAccessPath),
                            lhsPol,
                            pos = t.pos,
                            debug = AssignmentPolicyViolation
                        )
                        t.updateAttachment[PolicyStruct](lhsPol)
                    }

                case t@Select(qual, name) =>
                    traverse(qual)

                    if (!t.hasAttachment[PolicyStruct]) {
                        val accessPath: PolicyExpr = getPolicyStruct(qual).get.asInstanceOf[PolicyStructExpr].expr.join(ambientAccessPath)

                        t.symbol match {
                            case msym: Symbol if (msym.isMethod && msym.paramss.nonEmpty) || msym.isAnonymousFunction => {
                                val sig = getPolicyStruct(msym, withDefault=true).get.asInstanceOf[PolicySignature]

                                // parameterless method, treat like an apply
                                // TODO Rolph call handleCallsite from here?
                                if (msym.paramss.isEmpty) {
                                    t.updateAttachment[PolicyStruct](sig.returnPolicy.joinAccessPath(accessPath))

                                } else {
                                    t.updateAttachment[PolicyStruct](sig.joinAccessPath(accessPath))
                                }
                            }

                            // we can assume this is not a policy signature
                            case _ => {
                                val policy =
                                    getPolicyStruct(t.symbol)
                                        .getOrElse(PolicyStructExpr(lattice.bottom))
                                        .joinAccessPath(accessPath)

                                t.updateAttachment[PolicyStruct](policy)
                            }
                        }
                    }

                case t@If(cond, thenp, elsep) =>
                    traverse(cond)
                    val newPc: PolicyExpr = getPolicyStruct(cond).get.asInstanceOf[PolicyStructExpr].expr.join(pc)
                    val newLc: LabelChecker = LabelChecker(solver, newPc, codeSelectBound, ambientAccessPath)
                    newLc.traverse(thenp)
                    newLc.traverse(elsep)
                    if (!t.hasAttachment[PolicyStruct]) {
                        val thenLbl = getPolicyStruct(thenp).get
                        val elseLbl = getPolicyStruct(elsep).get
                        t.updateAttachment[PolicyStruct](thenLbl.join(elseLbl))
                    }

                case t@Match(selector, cases) =>
                    traverse(selector)
                    val selLbl = getPolicyStruct(selector).get.asInstanceOf[PolicyStructExpr].expr
                    val newPc: PolicyExpr = selLbl.join(pc)
                    var newLc: LabelChecker = LabelChecker(solver, newPc, codeSelectBound, ambientAccessPath)
                    // since cases are processed in order, patterns and guards in previous cases
                    //  leak information about the selector to later cases
                    var patgrdBound: PolicyExpr = newPc
                    var retPol: Option[PolicyStruct] = None

                    for {c@CaseDef(pat, guard, body) <- cases} {
                        // traverse pattern and guard
                        newLc.traverseMatchPattern(pat)
                        val patLbl: PolicyExpr =
                            getPolicyStruct(pat)
                                .flatMap { pat => Some(pat.asInstanceOf[PolicyStructExpr].expr) }
                                .getOrElse(lattice.bottom)

                        newLc.traverse(guard)
                        val guardLbl =
                            getPolicyStruct(guard)
                                .flatMap { pat => Some(pat.asInstanceOf[PolicyStructExpr].expr) }
                                .getOrElse(lattice.bottom)

                        patgrdBound = patgrdBound.join(patLbl).join(guardLbl)

                        // traverse case body
                        val casePc = newLc.pc.join(patgrdBound)
                        val caseLc = LabelChecker(solver, casePc, codeSelectBound, ambientAccessPath)
                        caseLc.traverse(body)

                        val bodyLbl = getPolicyStruct(body).get
                        val caseLbl = bodyLbl.joinAccessPath(casePc)

                        if (retPol.isEmpty) {
                            retPol = Some(caseLbl)
                        } else {
                            retPol = Some(retPol.get.join(caseLbl))
                        }

                        c.updateAttachment[PolicyStruct](caseLbl)
                    }

                    if (!t.hasAttachment[PolicyStruct]) {
                        // a match with no cases defaults to lattice.bottom
                        t.updateAttachment[PolicyStruct](retPol.getOrElse(PolicyStructExpr(lattice.bottom)))
                    }

                // TODO Rolph check for annotations on the policy signature for anonymous functions
                case t@Function(vparams,body) =>
                    traverseParams(vparams)

                    if (!t.hasAttachment[PolicyStruct]) {
                        val sym = t.symbol
                        val beginFreshVar = freshSolverVariable(sym.fullNameString + "$begin")
                        val begin = new SelectParam(beginFreshVar.name, Some(beginFreshVar))

                        val newLc: LabelChecker = LabelChecker(solver, begin.bound, begin.bound, ambientAccessPath)
                        newLc.traverse(body) // visit children
                        val bodyPol = getPolicyStruct(body).get

                        val sig =
                            PolicySignature(
                                accessPath = ambientAccessPath,
                                beginPolicy = begin,
                                paramssPolicies = List(vparams.map { vparam => getPolicyStruct(vparam).get }),
                                returnPolicy = bodyPol
                            )
                        t.symbol.updateAttachment[PolicyStruct](sig)
                        t.updateAttachment[PolicyStruct](sig)

                        // lower bound the begin label with the current pc, so that the function cannot escape
                        // and be called in more public contexts
                        policyStructFlowsTo(
                            PolicyStructExpr(pc.join(ambientAccessPath)),
                            PolicyStructExpr(begin.bound),
                            pos = t.pos,
                            debug = LambdaPCBeginPolicyViolation
                        )
                    }

                // TODO Rolph: Finish processing type application sites. For now, just propagate function's policies.
                case t@TypeApply(fun, args) =>
                    traverse(fun)

                    if (!t.hasAttachment[PolicyStruct]) {
                        t.updateAttachment[PolicyStruct](getPolicyStruct(fun).get)
                    }

                case t@Apply(fun, args) =>
                    // visit function and extract signature (with access path) from it
                    traverse(fun)

                    val sig = getPolicyStruct(fun).get.asInstanceOf[PolicySignature]

                    // increase ambient access path policy, since you can have "nested" access paths at call sites
                    // e.g., foo.barMethod(\x => x.bat())
                    // the access path policy for `\x => x.bat` should be at least foo
                    val newAccessPath = ambientAccessPath.join(sig.accessPath)
                    val newLc: LabelChecker = LabelChecker(solver, pc, codeSelectBound, newAccessPath)

                    // visit arguments
                    for ((param, arg) <- fun.symbol.tpe.params.zip(args)) {
                        // don't generate constraints for implicit params
                        // this is because Spark generates implementations of anonymous interfaces for
                        // implicit encoder params, which generates lots of constraints
                        if (param.isImplicit) {
                            arg.updateAttachment[PolicyStruct](defaultPolicyStruct(param))

                        } else {
                            newLc.traverse(arg)
                        }
                    }

                    if (!t.hasAttachment[PolicyStruct]) {
                        handleCallsite(t, fun, args)
                    }

                case t@This(_) =>
                    if (!t.hasAttachment[PolicyStruct]) {
                        val pol = getPolicyStruct(t.symbol).getOrElse(PolicyStructExpr(lattice.bottom))
                        t.updateAttachment[PolicyStruct](pol)
                    }

                case t@Super(qual, _) =>
                    super.traverse(t) // visit children
                    if (!t.hasAttachment[PolicyStruct]) {
                        val pol = getPolicyStruct(t.symbol).getOrElse(PolicyStructExpr(lattice.bottom))
                        t.updateAttachment[PolicyStruct](pol)
                    }

                case t@New(tpt) =>
                    traverse(tpt)
                    if (!t.hasAttachment[PolicyStruct]) {
                        val pol: PolicyStruct = getPolicyStruct(tpt).get
                        t.updateAttachment[PolicyStruct](pol)
                    }

                // TODO Rolph check DefDef and Function cases to make sure the rules for function defs are correct
                case t@DefDef(mods, name, tparams, vparamss, tpt, rhs) if !t.symbol.isSynthetic =>
                    if (!t.hasAttachment[PolicyStruct]) {
                        traverseParamss(vparamss)

                        // fetch policy from symbol
                        val pol = getPolicyStruct(t.symbol, withDefault=true).get

                        pol match {
                            case sig@PolicySignature(accessPath, begin, paramssPolicies, returnPolicy) =>
                                val newLc: LabelChecker = LabelChecker(solver, sig.beginPolicy.bound, sig.beginPolicy.bound, accessPath)
                                newLc.traverse(rhs) // visit children
                                val rhsPol = getPolicyStruct(rhs).get

                                policyStructFlowsTo(rhsPol, sig.returnPolicy, t.pos, debug = DefSigRHSPolicyViolation)
                                t.updateAttachment[PolicyStruct](sig)

                            // treat parameterless functions like valdefs
                            case PolicyStructExpr(polExpr) =>
                                traverse(rhs)
                                val rhsPol = getPolicyStruct(rhs).get.asInstanceOf[PolicyStructExpr].expr
                                policyStructFlowsTo(
                                    PolicyStructExpr(rhsPol.join(pc)),
                                    PolicyStructExpr(polExpr.toBound),
                                    pos = t.pos,
                                    debug = DefExprRHSPolicyViolation
                                )
                        }
                    }

                // skip synthetic defs
                case t@DefDef(mods, name, tparams, vparamss, tpt, rhs) if t.symbol.isSynthetic => ()

                // TODO Rolph labelchecking for exceptional control flow is not yet supported
                // set exception policy to most restrictive (top) for now
                case t@Throw(e) =>
                    super.traverse(t)
                    if (!t.hasAttachment[PolicyStruct]) {
                        t.updateAttachment[PolicyStruct](PolicyStructExpr(lattice.top))
                    }

                // make sure expr policy flows to policy in type ascription
                case t@Typed(expr, tpt) =>
                    super.traverse(t) // visit children
                    val exprPol = getPolicyStruct(expr).get
                    val ascribedPol: Option[PolicyStruct] = getPolicyStruct(tpt.symbol)

                    val pol: PolicyStruct =
                        if (ascribedPol.isEmpty) {
                            exprPol

                        } else {
                            policyStructFlowsTo(exprPol, ascribedPol.get, pos = t.pos, debug = AscribedPolicyViolation)
                            ascribedPol.get
                        }
                    t.updateAttachment(pol)

                case t@Block(stmts, expr) =>
                    // TODO: we should be more careful about side-effects in blocks used as expressions
                    // (eg. inside short-circuited bool expressions)
                    super.traverse(t) // visit children
                    val exprPol: PolicyStruct = getPolicyStruct(expr).getOrElse(PolicyStructExpr(lattice.bottom))
                    t.updateAttachment[PolicyStruct](exprPol)

                case t@Return(expr) =>
                    super.traverse(t)
                    t.updateAttachment[PolicyStruct](getPolicyStruct(expr).get)

                // don't analyze import statements!
                case t@Import(_, _) => ()

                case _ => super.traverse(tree)
            }

        }

        /** Traverse a pattern match tree. */
        def traverseMatchPattern(tree: Tree)(implicit lattice: SecLattice): Unit = {
            tree match {
                // extract class identifier to get policy of
                case t@Apply(fun, _) => {
                    if (!t.hasAttachment[PolicyStruct]) {
                        val pol = getPolicyStruct(fun.tpe.resultType.typeSymbol, withDefault=true).get
                        t.updateAttachment[PolicyStruct](pol)
                    }
                }

                // default policy for a match is bottom
                case _ => {
                    if (!tree.hasAttachment[PolicyStruct]) {
                        tree.updateAttachment[PolicyStruct](PolicyStructExpr(lattice.bottom))
                    }
                }
            }
        }
    }
}

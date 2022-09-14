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

import com.facebook.flowframe.purpose.{PurposeParser, PurposePolicies}
import org.scalactic.anyvals.PosZInt
import org.scalatest.matchers.should.Matchers._
import org.scalatest.prop.{Generator, GeneratorDrivenPropertyChecks}
import org.scalatest.propspec.AnyPropSpec

import java.io.PrintWriter
import scala.collection.immutable._
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{GenericRunnerSettings, Global}

import scala.collection.mutable

class SolverSpec extends AnyPropSpec with GeneratorDrivenPropertyChecks with PurposeParser {
	val settings = new GenericRunnerSettings(msg => sys.error(msg))
	settings.embeddedDefaults(Thread.currentThread().getContextClassLoader)
	val reporter = new ConsoleReporter(settings, Console.in, new PrintWriter(Console.out))
	val global : Global = new Global(settings, reporter)

	implicit val cache = new mutable.LinkedHashMap[List[DisList], List[(Set[DisList], ClauseSet)]]()
	implicit override val generatorDrivenConfig =
		PropertyCheckConfiguration(minSize = 1)

	//override def overrideParameters(p: Test.Parameters): Test.Parameters = {
	//  p.withMinSuccessfulTests(15)
	//    .withMaxDiscardRatio(20)
	//}

	def solverVars: Generator[BasePolicy] = for {
	 name <- specificValues("x", "y", "z")
	} yield freshSolverVariable(name)

	//def argVars: Generator[BasePolicy] = for {
	//  name <- specificValues("A", "B", "C")
	//} yield ArgVar(name,NoPosition)

	def base_policies: Generator[BasePolicy] = evenly(solverVars, specificValues(Always, Never))

	def genPol0(maxDepth: PosZInt): Generator[PolicyExpr] =
		if (maxDepth == PosZInt(0)) base_policies
		else
		evenly(base_policies,
			genNT(And, maxDepth),
			genNT(Or, maxDepth)
		)

	def genNT(ctor: (PolicyExpr, PolicyExpr) => PolicyExpr, maxDepth: PosZInt): Generator[PolicyExpr] = for {
		depthL <- posZIntsBetween(0, PosZInt.from(maxDepth - 1).getOrElse(0))
		depthR <- posZIntsBetween(0, PosZInt.from(maxDepth - 1).getOrElse(0))
		left <- genPol0(depthL)
		right <- genPol0(depthR)
	} yield ctor(left, right)

	implicit val policies: Generator[PolicyExpr] = genPol0(1)

	def wnorms(csize: PosZInt, dsize:PosZInt): Generator[WNorm] = for {
		b <- base_policies
		// p <- [[[ predicate generator ]]](csize, dsize)
	} yield WNorm(b, NormTrue)

	def jnorms(jsize: PosZInt, csize: PosZInt, dsize: PosZInt): Generator[JNorm] = for {
		ns <- Generator.setGenerator[WNorm](wnorms(csize, dsize)).havingSizesBetween(0, jsize)
	} yield JNorm(ns)

	def mnorms(msize: PosZInt, jsize:PosZInt, csize:PosZInt, dsize:PosZInt): Generator[MNorm] = for {
		ds <- Generator.setGenerator[JNorm](jnorms(jsize, csize, dsize)).havingSizesBetween(0,msize)
	} yield MNorm(ds)

	implicit val normPolicies: Generator[NormalizedPolicy] = mnorms(1, 3, 0, 0)

	def assertEquivalence(p: PolicyExpr, q: PolicyExpr): Boolean = {
		lattice.flowsto(p, q) && lattice.flowsto(q, p)
	}

	def extractBasePolicies(p: NormalizedPolicy): Set[Set[WNorm]] = p match {
		case MNorm(disjs) => for {JNorm(conjs) <- disjs} yield conjs
	}

	def maxClauseLength(p: PolicyExpr, q: PolicyExpr, len: Int): Boolean = {
		val plen = extractBasePolicies(normalize(p)).size
		val qlen = extractBasePolicies(normalize(q)).size
		plen + qlen <= len
	}

	property("normIdemp") {
		forAll { (p: PolicyExpr) =>
			assertEquivalence(convert(normalize(p)), p) should be (true)
		}
	}

	property("And_commutative") {
		forAll { (p: PolicyExpr, q: PolicyExpr) =>
				whenever(maxClauseLength(And(p, q), And(q, p), 10)) {
					assertEquivalence(And(p, q), And(q, p)) should be (true)
				}
			}
	}

	property("Or_commutative") {
		forAll { (p: PolicyExpr, q: PolicyExpr) =>
			whenever(maxClauseLength(Or(p, q), Or(q, p), 10)) {
				assertEquivalence(Or(p, q), Or(q, p)) should be (true)
			}
		}
	}

	property("And_assoc") {
		forAll { (p: PolicyExpr, q: PolicyExpr, r: PolicyExpr) =>
				whenever(maxClauseLength(And(And(p, q), r), And(p, And(q, r)), 10)) {
					assertEquivalence(And(And(p, q), r), And(p, And(q, r))) should be (true)
			}
		}
	}

	property("Or_assoc") {
		forAll { (p: PolicyExpr, q: PolicyExpr, r: PolicyExpr) =>
				whenever(maxClauseLength(Or(Or(p, q), r), Or(p, Or(q, r)), 10)) {
					assertEquivalence(Or(Or(p, q), r), Or(p, Or(q, r))) should be (true)
				}
		}
	}

	property("And_distrib") {
		forAll { (p: PolicyExpr, q: PolicyExpr, r: PolicyExpr) => {
				val left = And(p, Or(q, r))
				val right = Or(And(p, q), And(p, r))
				whenever(maxClauseLength(left, right, 10)) {
					assertEquivalence(left, right) should be (true)
				}
			}
		}
	}

	property("Or_distrib") {
		forAll { (p: PolicyExpr, q: PolicyExpr, r: PolicyExpr) =>
				val left = And(Or(p, q), Or(p, r))
				val right = Or(p, And(q, r))
				whenever(maxClauseLength(left, right, 10)) {
					assertEquivalence(left, right) should be (true)
				}
		}
	}
}

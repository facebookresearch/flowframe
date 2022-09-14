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

import com.facebook.flowframe.purpose.predicates.Predicates
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Properties, _}
import scala.collection.mutable

class PredicateSpec extends Properties("Policy") with Predicates {
	// TODO Rolph: Commenting this out for now since it is pegging the CPU for some reason
	/*
	implicit val cache = new mutable.LinkedHashMap[List[DisList], List[(Set[DisList], ClauseSet)]]()

	override def overrideParameters(p: Test.Parameters): Test.Parameters = {
	p.withMinSuccessfulTests(15)
	  .withMaxDiscardRatio(20)
	}

	protected val vars = for {
	name <- Gen.oneOf("A", "B", "C") // Gen.identifier // Gen.oneOf("__foo", "__bar", "__baz", "__qux")
	} yield Variable(name)

	protected val purposeBools: Gen[Literal] = for {
	b <- Gen.oneOf(true, false)
	} yield PurposeBool(b)

	protected val purposeInts: Gen[Literal] = for {
	n <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
	} yield PurposeInt(n.toInt)

	protected val purposeFloats: Gen[Literal] = for {
	n <- Gen.chooseNum(Float.MinValue, Float.MaxValue)
	} yield PurposeFloat(n.toFloat)

	protected val purposeStrs: Gen[Literal] = for {
	s <- Gen.asciiPrintableStr
	} yield PurposeString(s)

	protected val purposeLits: Gen[Literal] = Gen.oneOf(purposeBools, purposeInts, purposeFloats, purposeStrs)

	protected val ops: Gen[Operation] = Gen.oneOf(EQ, GT, GTE, LT, LTE)

	protected val atoms = Gen.oneOf(vars, purposeLits)

	val binOps = for {
	lhs <- vars
	// op <- ops
	// rhs <- purposeLits
	} yield BinOp(lhs, EQ, PurposeBool(true))

	def genPred0(maxDepth: Int): Gen[Predicate] =
	if (maxDepth == 0) binOps
	else Gen.oneOf(binOps,
	  genNT(LAnd, maxDepth),
	  genNT(LOr, maxDepth),
	  genNT((p, _) => LNot(p), maxDepth)
	)

	def genNT(ctor: (Predicate, Predicate) => Predicate, maxDepth: Int): Gen[Predicate] = for {
	depthL <- Gen.choose(0, maxDepth - 1)
	depthR <- Gen.choose(0, maxDepth - 1)
	left <- genPred0(depthL)
	right <- genPred0(depthR)
	} yield ctor(left, right)

	val predicates: Gen[Predicate] = Gen.sized { size => genPred0(size) }

	val npreds = for {
	neg <- Gen.oneOf[Boolean](true, false)
	binop <- binOps
	} yield NPred(binop, neg)

	def dpreds(maxSize: Int) = for {
	ns <- Gen.containerOfN[Set,NPred](maxSize, npreds)
	} yield DPred(ns)

	def cpreds(csize: Int, dsize:Int): Gen[NormalizedPred] = for {
	ds <- Gen.containerOfN[Set,DPred](csize, dpreds(dsize))
	} yield CPred(ds)

	val normPredicates: Gen[NormalizedPred] = cpreds(1, 3)
	// Gen.sized { size => cpreds(size) }

	implicit lazy val arbitraryPred: Arbitrary[Predicate] = Arbitrary(predicates)
	implicit lazy val arbitraryAtom: Arbitrary[Atom] = Arbitrary(atoms)
	implicit lazy val arbitraryNormPred: Arbitrary[NormalizedPred] = Arbitrary(normPredicates)

	def extractClauses(p: NormalizedPred): Set[Set[NPred]] = p match {
	case CPred(conjs) => for {DPred(disjs) <- conjs} yield disjs
	}

	def assertFalse(p: Predicate): Boolean = {
	val c1 = normalizePred(p)
	if (c1 == False) return true

	val c1Clauses = extractClauses(c1)
	isUnsat(c1Clauses.toList) match {
	  case Some(_) => true
	  case _ => false
	}
	}

	def assertEquivalence(p: Predicate, q: Predicate): Boolean = {
	val c1 = normalizePred(p)
	if (c1 == False) return assertFalse(q)

	val c2 = normalizePred(q)
	if (c2 == False) return assertFalse(p)

	val negatedC2 = normalizePred(LNot(convertPred(c2)))
	if (negatedC2 == False) return true

	val c1Clauses = extractClauses(c1).toList
	val neg_c2Clauses = extractClauses(negatedC2).toList

	isUnsat(c1Clauses ++ neg_c2Clauses) match {
	  case Some(_) => true
	  case _ => false
	}
	}

	property("normIdemp") =
	forAll { (p: Predicate) =>
	  normalizePred(convertPred(normalizePred(p))) == normalizePred(p)
	}

	def maxClauseLength(p: Predicate, q: Predicate, len: Int): Boolean = {
	val c1len = normalizePred(p) match {
	  case False => 0
	  case c1 => extractClauses(c1).size
	}
	val c2 = normalizePred(q)
	val c2len = c2 match {
	  case False => 0
	  case _ => normalizePred(LNot(convertPred(c2))) match {
		case False => 0
		case c2 => extractClauses(c2).size
	  }
	}
	c1len + c2len <= len
	}

	propertyWithSeed("LAnd_commutative", None) =
	forAll { (p: Predicate, q: Predicate) =>
	  maxClauseLength(LAnd(p, q), LAnd(q, p), 10) ==> {
		assertEquivalence(LAnd(p, q), LAnd(q, p))
	  }
	}

	propertyWithSeed("LOr_commutative", None) =
	forAll { (p: Predicate, q: Predicate) =>
	  maxClauseLength(LOr(p, q), LOr(q, p), 10) ==> {
		assertEquivalence(LOr(p, q), LOr(q, p))
	  }
	}

	propertyWithSeed("LEq_commutative", None) =
	forAll { (a1: Atom, a2: Atom) =>
	  assertEquivalence(BinOp(a1, EQ, a2), BinOp(a2, EQ, a1))
	}

	propertyWithSeed("LAnd_assoc", None) =
	forAll { (p: Predicate, q: Predicate, r: Predicate) =>
	  maxClauseLength(LAnd(LAnd(p, q), r), LAnd(p, LAnd(q, r)), 10) ==>
		assertEquivalence(LAnd(LAnd(p, q), r), LAnd(p, LAnd(q, r)))
	}

	propertyWithSeed("LOr_assoc", None) =
	forAll { (p: Predicate, q: Predicate, r: Predicate) =>
	  maxClauseLength(LOr(LOr(p, q), r), LOr(p, LOr(q, r)), 10) ==>
		assertEquivalence(LOr(LOr(p, q), r), LOr(p, LOr(q, r)))
	}
	propertyWithSeed("LAnd_distrib", None) =
	 forAll { (p: Predicate, q: Predicate, r: Predicate) =>
	   val left = LAnd(p, LOr(q, r))
	   val right = LOr(LAnd(p, q), LAnd(p, r))
	   maxClauseLength(left, right , 10) ==> assertEquivalence(left,right)
	 }
	propertyWithSeed("LOr_distrib", None) =
	forAll { (p: Predicate, q: Predicate, r: Predicate) =>
	  val left = LAnd(LOr(p, q), LOr(p, r))
	  val right = LOr(p, LAnd(q, r))
	  maxClauseLength(left, right , 10) ==> assertEquivalence(left,right)
	}

	propertyWithSeed("demorgans", None) =
	forAll { (p: Predicate, q: Predicate) =>
	  val left = LNot(LAnd(p, q))
	  val right = LOr(LNot(p), LNot(q))
	  maxClauseLength(left, right , 10) ==> assertEquivalence(left,right)
	}

	propertyWithSeed("contradiction", None) =
	forAll { (p: Predicate) =>
	  assertFalse(LAnd(p, LNot(p)))
	}

	propertyWithSeed("negation", None) =
	forAll { (p: Predicate) =>
	  val left = LNot(LNot(p))
	  val right = p
	  maxClauseLength(left, right ,5) ==> assertEquivalence(left,right)
	}

	propertyWithSeed("excludedMiddle", None) =
	forAll { (p: Predicate) =>
	  val left = LOr(LNot(p),p)
	  val right = True
	  maxClauseLength(left, right ,10) ==> assertEquivalence(left,right)
	}
	*/
}

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

/** Policy Signatures for Spark operations. */
trait SparkSignatures[T <: PolicyLang] extends DefaultPolicies[T] {
	val global: Global
	import global._

	implicit val lattice : SecLattice

	lazy val DatasetClass: ClassSymbol = rootMirror.staticClass("org.apache.spark.sql.Dataset")
	lazy val DatasetType: Type = rootMirror.staticClass("org.apache.spark.sql.Dataset").info.typeSymbol.tpe
	lazy val SparkSessionClass: ClassSymbol = rootMirror.staticClass("org.apache.spark.sql.SparkSession")
	lazy val DataFrameWriterClass: ClassSymbol = rootMirror.staticClass("org.apache.spark.sql.DataFrameWriter")
	lazy val DataFrameReaderClass: ClassSymbol = rootMirror.staticClass("org.apache.spark.sql.DataFrameReader")

	/** **
	 * Special handling for some important symbols.  Eventually we should make it possible for the
	 * user to create these via annotations and signature files.
	 */
	/** * * API functions */
	//
	/**
	 * The symbol for
	 * org.apache.spark.sql.Dataset.filter(func: T => Boolean): Dataset[T]
	 */
	lazy val filterSym: Symbol = {
		val filterSym = try {
			definitions.getMemberMethod(DatasetClass, encode("filter"))
				.asTerm.alternatives.filter(sym => {
				sym.info.paramTypes.length == 1 && definitions.isFunctionType(sym.info.paramTypes(0))
			})(0)
		} catch {
			case (e: Throwable) => {
				error("Exception: " + e.getMessage)
				throw e
			}
		}

		// create signature for filter function:
		// ds: Dataset[U]
		// ds.filter{this}(f:(T{this} => Boolean{P})): Dataset[T]{this join P join ClassLblOf(U)}
		val predSym = filterSym.paramss(0)(0)
		val predVar = freshSolverVariable(nameForParam(predSym), predSym.pos)
		val predVarBegin = freshSolverVariable(nameForParam(predSym) + "$begin", predSym.pos)
		val predVarReturn = freshSolverVariable(nameForParam(predSym) + "$return", predSym.pos)
		val predSig =
			PolicySignature(
				lattice.bottom,
				new SelectParam(predVarBegin.name, upperBound=Some(predVarBegin)),
				List(List(
					PolicyStructExpr(new ArgParam(nameForParam(predSym), predSym, Some(predVar)))
				)),
				PolicyStructExpr(new ArgParam(predVarReturn.name, predSym, Some(predVarReturn)))
			)

		val filterVarBegin = freshSolverVariable(filterSym.fullNameString + "$begin", filterSym.pos)
		val filterSig =
			PolicySignature(
				lattice.bottom,
				new SelectParam(filterVarBegin.name, upperBound=Some(filterVarBegin)),
				List(List(predSig)),
				PolicyStructExpr(PolicyExprRef(ArgPolicySigRef(predSym.name, PolicySigReturnRef)))
			)

		filterSym.updateAttachment[PolicyStruct](filterSig)
		predSym.updateAttachment[PolicyStruct](predSig)
		filterSym
	}

	/**
	 * The symbol for
	 * org.apache.spark.sql.Dataset.map[U : Encoder](func: T => U): Dataset[U]
	 */
	lazy val mapSym: Symbol = {
		val mapSym = try {
			definitions.getMemberMethod(DatasetClass, encode("map"))
				.asTerm.alternatives.filter(sym => {
				sym.info.paramTypes.length == 1 && definitions.isFunctionType(sym.info.paramTypes(0))
			})(0)
		} catch {
			case (e: Throwable) => {
				error("Exception: " + e.getMessage)
				throw e
			}
		}

		// create signature for map function:
		// map{this}(f:(T{this} => U{P})): Dataset[U]{this join P}
		val opSym = mapSym.paramss(0)(0)
		val opVarBegin = freshSolverVariable(nameForParam(opSym) + "$begin", opSym.pos)
		val opVarReturn = freshSolverVariable(nameForParam(opSym) + "$return", opSym.pos)
		val opArgVar = freshSolverVariable(nameForParam(opSym) + " $row", opSym.pos)
		val opSig = PolicySignature(
			lattice.bottom,
			new SelectParam(opVarBegin.name, upperBound=Some(opVarBegin)),
			List(List(
				PolicyStructExpr(new ArgParam(opArgVar.name, opSym, Some(opArgVar)))
			)),
			PolicyStructExpr(new ArgParam(opVarReturn.name, opSym, Some(opVarReturn)))
		)

		val evSym = mapSym.paramss(1)(0)
		val mapVarBegin = freshSolverVariable(mapSym.fullNameString + "$begin", mapSym.pos)
		val mapSig = PolicySignature(
			lattice.bottom,
			new SelectParam(mapVarBegin.name, upperBound=Some(mapVarBegin)),
			List(
				List(opSig),
				List(PolicyStructExpr(new ArgParam(nameForParam(evSym), evSym, Some(freshSolverVariable(nameForParam(evSym), evSym.pos)))))
			),
			PolicyStructExpr(PolicyExprRef(ArgPolicySigRef(opSym.name, PolicySigReturnRef)))
		)
		mapSym.updateAttachment[PolicyStruct](mapSig)
		opSym.updateAttachment[PolicyStruct](opSig)
		mapSym
	}

	/*
	/**
	 * The symbol for
	 * org.apache.spark.sql.SparkSession.table(String)
	 */
	lazy val tableSym: Symbol = definitions.getMemberMethod(SparkSessionClass, encode("table"))
		.asTerm.alternatives.filter(_.info.paramTypes(0).=:=(definitions.StringTpe))(0)

	/**
	 * The symbol(s) for
	 * org.apache.spark.sql.SparkSession.sql(String)
	 */
	lazy val sqlSym: Symbol = definitions.getMemberMethod(SparkSessionClass, encode("sql"))
		.asTerm.alternatives.filter(_.info.paramTypes(0).=:=(definitions.StringTpe))(0)

	lazy val readCsvSym: Symbol = definitions.getMemberMethod(DataFrameReaderClass, encode("csv"))
		.asTerm.alternatives.filter(_.info.paramTypes(0).=:=(definitions.StringTpe))(0)

	lazy val asSym: Symbol = {
		val sym = definitions.getMemberMethod(DatasetClass, encode("as"))
		val opSym = mapSym.paramss.head.head

		val asSig = PolicySignature(
			lattice.bottom,
			new SelectParam(sym.owner),
			List(List(
				PolicyStructExpr(new ArgParam(nameForParam(opSym), opSym, Some(freshSolverVariable(nameForParam(opSym), opSym.pos))))
			)),
			PolicyStructExpr(
				PolicyExprRef(ArgPolicyRef(opSym.name)).join(PolicyExprRef(ReceiverRef))
			)
		)
		sym.updateAttachment[PolicyStruct](asSig)
		sym
	}

	lazy val saveAsTableSym: Symbol = definitions.getMemberMethod(DataFrameWriterClass, encode("saveAsTable"))
		.asTerm.alternatives.filter(_.info.paramTypes(0).=:=(definitions.StringTpe))(0)

	lazy val writeCsvSym: Symbol = definitions.getMemberMethod(DataFrameWriterClass, encode("csv"))
		.asTerm.alternatives.filter(_.info.paramTypes(0).=:=(definitions.StringTpe))(0)
	*/

	lazy val sparkSignatures: Set[Symbol] = Set(filterSym, mapSym)
}

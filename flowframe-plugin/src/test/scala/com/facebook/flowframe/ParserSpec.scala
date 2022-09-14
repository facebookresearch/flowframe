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

import com.facebook.flowframe.purpose.PurposeParser
import org.scalacheck.Prop.propBoolean
import org.scalacheck._

import java.io.PrintWriter
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{GenericRunnerSettings, Global}

class ParserSpec extends Properties ("Parser") with PurposeParser {
  val settings = new GenericRunnerSettings(msg => sys.error(msg))
  settings.embeddedDefaults(Thread.currentThread().getContextClassLoader)
  val reporter = new ConsoleReporter(settings, Console.in, new PrintWriter(Console.out))
  val global : Global = new Global(settings, reporter)

  override def overrideParameters(p: Test.Parameters): Test.Parameters = {
	p.withMinSuccessfulTests(15)
	  .withMaxDiscardRatio(20)
  }

  def canParse(str:String):Boolean = {
	parseAll(policy_expr, str) match {
	  case Success(ast,_) => true
	  case Failure(msg,_) => println("Failure: " + msg); false //throw SyntaxError(msg)
	  case _ => false //throw SyntaxError(msg)
	}
  }

  def parseBinop(str:String):Boolean = {
	parseAll(policy_with_compare, str) match {
	  case Success(ast, next) => true
	  case Failure(msg,next) => println("Failure: " + msg); false //throw SyntaxError(msg)
	  case _ => false //throw SyntaxError(msg)
	}
  }

  propertyWithSeed("parse_any", None) = { canParse("any") }

  propertyWithSeed("parse_always", None) = { canParse("always") }

  propertyWithSeed("parse_none", None) = { canParse("none") }

  propertyWithSeed("parse_never", None) = { canParse("never") }

  propertyWithSeed("parse_purpose", None) = {
	canParse("A :: B")
  }

  propertyWithSeed("parse_binop", None) = {
	parseBinop("a = \"str\"")
  }

  propertyWithSeed("parse_with", None) = {
	canParse("A :: V with (a = \"str\")")
  }

  propertyWithSeed("parse_with_or1", None) = {
	val policyStr: String = "A::V " +
	  "with (model_version = \"V\") or B"
	canParse(policyStr)
  }

  propertyWithSeed("parse_with_or2", None) = {
	val policyStr: String = "A::V " +
	  "with (model_version = \"V\") or B::X "
	canParse(policyStr)
  }

  propertyWithSeed("parse_nested2", None) = {
	val policyStr: String = "A::X " +
	  "with (model_version = \"X\") " +
	  "or (B::X " +
	  "or B::Y::Y::Y) " +
	  "with (model_version = \"Y\")"
	canParse(policyStr)
  }

  propertyWithSeed("parse_big", None) = {
	val policyStr: String = "A::Y " +
	  "with (model_version = \"Y\") " +
	  "or (B::Y " +
		   "or B::C or B::C::Y " +
		   "or B::C::Y::YF " +
		   "or B::C::G " +
		   "or B::C::G::GF " +
		   "or B::S:: F :: F ) " +
		  "with (model_version = \"F\") " +
	  "or B::C::F with (model_version = \"P\") " +
	  "or B::X with (model_version = \"E\") " +
	  "or any with not (model_version = \"E\" " +
					   "or model_version = \"E\" " +
					   "or model_version = \"E\" " +
					   "or model_version = \"E\")"
	canParse(policyStr)
  }
}

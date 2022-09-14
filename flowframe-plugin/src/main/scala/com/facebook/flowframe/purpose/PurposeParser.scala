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

package com.facebook.flowframe.purpose

import com.facebook.flowframe.SyntaxError

import scala.tools.nsc.Global
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/** A parser for Purpose Policies. */
trait PurposeParser extends JavaTokenParsers with PurposePolicies {
    override val global: Global
    import global._

    lazy val PolicyAnnotation: global.ClassSymbol = rootMirror.getRequiredClass("com.facebook.flowframe.Policy")
    lazy val PolicyStructAnnotation: global.ClassSymbol = rootMirror.getRequiredClass("com.facebook.flowframe.PolicyStruct")

    // TODO Rolph: add support for annotating policy signatures
    override def getPolicyStruct(anns: List[PurposeParser.this.global.AnnotationInfo]): Option[PolicyStruct] = {
        for {ann <- anns filter (_ matches PolicyAnnotation)
             (n, arg) <- ann.javaArgs}
        yield {
            if (n.toString.equals("value")) {
                arg match {
                    case LiteralAnnotArg(const) =>
                        return Some(PolicyStructExpr(parsePolicy(const.stringValue)))

                    case _ => throw SyntaxError("Unexpected argument type")
                }
            }
        }

        None
    }

    def parsePolicy(str: String): PolicyExpr = {
        parseAll[PolicyExprNode](policy_expr, str) match {
            case Success(ast, _) => ast.asInstanceOf[PolicyExprNode].policyValue
            case Failure(fail, _) => throw SyntaxError(fail)
            case s => throw SyntaxError("Unknown parsing error: " + s)
        }
    }

    def name_pattern: Regex = """(?!any|always|none|never|not|and|or|with)[a-zA-Z_][a-zA-Z0-9_]*""".r

    def comp: Parser[Component] = name_pattern ^^ { case c => Component(c) }

    def purpose: Parser[PurposeNode] = comp ~ rep("::" ~ comp) ^^ {
        case c ~ cs =>
            cs match {
                case x: List[String ~ Component] => PurposeNode(c, x)
            }
    }

    def policy_expr: Parser[PolicyExprNode] = policy_term ~ rep("or" ~ policy_term) ^^ {
        case p1 ~ ors => ors match { // TODO: compiler warns of inexhaustive match here
            case Nil => p1
            case ("or" ~ _) :: _ => OrPolicyNode(p1, ors)
        }
    }

    def policy_term: Parser[PolicyExprNode] =
        (policy_factor ~ "with" ~ policy_with_compare
            ^^ { case p ~ "with" ~ pred => WithNode(p, pred) }

            | policy_factor ~ rep("and" ~ policy_factor) ^^ {
            case p ~ ands => ands match {
                case ands: List[String ~ PolicyExprNode] => AndPolicyNode(p, ands)
            }
        })

    def policy_factor: Parser[PolicyExprNode] =
        (("any" | "always") ^^ { _ => AlwaysNode() }
            | ("none" | "never") ^^ { _ => NeverNode() }
            | purpose ^^ { case p: PurposeNode => p }
            | "(" ~ policy_expr ~ ")" ^^ { case "(" ~ p ~ ")" => Parentheses(p) }
            | "?" ~ "configerator_ref" ~ "(" ~ stringLiteral ~ "," ~ "[" ~ stringLiteral ~ rep("," ~ stringLiteral) ~ "]" ~ ")" ^^ {
            case "?" ~ "configerator_ref" ~ "(" ~ _ ~ "," ~ "[" ~ _ ~ _ ~ "]" ~ ")" =>
                throw new UnsupportedOperationException("configerator_ref is not (yet) supported")
        }
            | "not" ~ policy_factor ^^ { case _ ~ p => NotNode(p) }
            )

    def policy_with_expr: Parser[PredicateNode] = policy_with_term ~ rep("or" ~ policy_with_term) ^^ {
        case t1 ~ ors => ors match {
            case ors: List[String ~ PredicateNode] => OrPredicateNode(t1, ors)
        }
    }

    def policy_with_term: Parser[PredicateNode] = policy_with_compare ~ rep("and" ~ policy_with_compare) ^^ {
        case t1 ~ ands => ands match {
            case ands: List[String ~ PredicateNode] => AndPredicateNode(t1, ands)
        }
    }

    def policy_with_compare: Parser[PredicateNode] =
        (policy_with_factor ~ rep(op ~ policy_with_factor) ^^ {
            case a1 ~ rs => rs match {
                case oa2: List[String ~ AtomNode] => BinOpPredicateNode(a1, oa2)
            }
        }
            | "(" ~ policy_with_expr ~ ")" ^^ { case "(" ~ p ~ ")" => PredParentheses(p) }
            | "not" ~ policy_with_compare ^^ { case "not" ~ p => NotPredicateNode(p) }
            )

    def policy_with_factor: Parser[AtomNode] =
        (name_pattern ^^ { p => VariableNode(p) }
            | stringLiteral ^^ { s => StringLiteral(s) }
            | wholeNumber ^^ { n => IntegerLiteral(n.toInt) }
            | floatingPointNumber ^^ { n => IntegerLiteral(n.toInt) }
            | "true" ^^ { _ => BooleanLiteral(true) }
            | "false" ^^ { _ => BooleanLiteral(false) }
            | "create_set" ~ "(" ~ policy_with_expr ~ rep("," ~ policy_with_expr) ~ ")"
            ^^ { case "create_set" ~ "(" ~ _ ~ _ ~ ")" => throw new UnsupportedOperationException("create_set is not (yet) supported") }
            | "?" ~ "configerator_ref" ~ "(" ~ stringLiteral ~ "," ~ "[" ~ stringLiteral ~ rep("," ~ stringLiteral) ~ "]" ~ ")" ^^ {
            case "?" ~ "configerator_ref" ~ "(" ~ _ ~ "," ~ "[" ~ _ ~ _ ~ "]" ~ ")" =>
                throw new UnsupportedOperationException("configerator_ref is not (yet) supported")
        }
            | "contains" ~ "(" ~ policy_with_expr ~ "," ~ policy_with_expr ~ ")"
            ^^ { case "contains" ~ "(" ~ _ ~ "," ~ _ ~ ")" => throw new UnsupportedOperationException("contains is not (yet) supported") }
            | "is_set" ~ "(" ~ name_pattern ~ ")"
            ^^ { case "is_set" ~ "(" ~ _ ~ ")" => throw new UnsupportedOperationException("is_set is not (yet) supported") }
            )

    def op: Parser[String] = "and" | "or" | ">" | ">=" | "<" | "<=" | "="

    /**
     * A Purpose Policy AST node.
     */
    trait PolicyExprNode {
        def policyValue : PolicyExpr
    }

    /**
     * AST node for a single purpose or variable.
     */
    trait BasePolicyNode extends PolicyExprNode

    /**
     * A component of a purpose is an alpha-numeric string that begins with a letter and is not
     * a Purpose Policy reserved keyword: {always|never|not|and|or|with|any|none}
     */
    case class Component(x: String) {
        def value: String = {
            val name = name_pattern
            x match {
                case name(_*) => x
                case _ => throw new RuntimeException("Syntax error: purpose contains illegal keyword or characters.")
            }
        }
    }

    /**
     * An AST node for purposes
     */
    case class PurposeNode(c:Component, cs:List[String~Component]) extends BasePolicyNode {
        def component(c: String~Component): String = c match {
            case "::"~x => x.value
        }
        def policyValue:Purpose = Purpose(c.value +: cs.map(component))
    }

    case class AlwaysNode() extends BasePolicyNode {
        def policyValue: PolicyExpr = Always
    }
    case class NeverNode() extends BasePolicyNode {
        def policyValue: PolicyExpr = Never
    }

    case class Parentheses(p: PolicyExprNode) extends BasePolicyNode {
        def policyValue: PolicyExpr = p.policyValue
    }

    case class NotNode(p: PolicyExprNode) extends PolicyExprNode {
        def policyValue: PolicyExpr = Not(p.policyValue)
    }

    case class OrPolicyNode(b:PolicyExprNode, ps: List[String~PolicyExprNode]) extends PolicyExprNode {
        def policyValue: PolicyExpr = {
            ps.foldLeft(b.policyValue)((acc, p) => p match {
                case "or"~x => Or(acc,x.policyValue)
            })
        }
    }

    case class AndPolicyNode(b:PolicyExprNode, ps: List[String~PolicyExprNode]) extends PolicyExprNode {
        def policyValue:PolicyExpr = {
            ps.foldLeft(b.policyValue)((acc, p) => p match {
                case "and"~x => And(acc,x.policyValue)
            })
        }
    }

    case class WithNode(b:PolicyExprNode, p: PredicateNode) extends PolicyExprNode {
        def policyValue:PolicyExpr = {
            With(b.policyValue, p.predValue)
        }
    }

    case class NotPredicateNode(p: PredicateNode) extends PredicateNode {
        def predValue:Predicate = {
            LNot(p.predValue)
        }
    }

    /**
     * AST nodes for predicates
     */
    trait PredicateNode {
        def predValue : Predicate
    }

    trait AtomNode {
        def atomValue : Atom
    }

    trait BasePredicateNode extends PredicateNode

    case class PredParentheses(p: PredicateNode) extends BasePredicateNode {
        def predValue: Predicate = p.predValue
    }

    case class BinOpPredicateNode(a1: AtomNode, oa2:List[String~AtomNode]) extends BasePredicateNode {
        def predValue: Predicate = {
            oa2.head match {
                case "=" ~ a2 => BinOp(a1.atomValue, EQ, a2.atomValue)
                case ">" ~ a2 => BinOp(a1.atomValue, GT, a2.atomValue)
                case ">=" ~ a2 => BinOp(a1.atomValue, GTE, a2.atomValue)
                case "<" ~ a2 => BinOp(a1.atomValue, LT, a2.atomValue)
                case "<=" ~ a2 => BinOp(a1.atomValue, LTE, a2.atomValue)
                // TODO: warn if oa2.length > 1 ???
            }
        }
    }

    case class OrPredicateNode(b:PredicateNode, ps: List[String~PredicateNode]) extends PredicateNode {
        def predValue:Predicate= {
            ps.foldLeft(b.predValue)((acc, p) => p match {
                case "or"~x => LOr(acc,x.predValue)
            })
        }
    }

    case class AndPredicateNode(b:PredicateNode, ps: List[String~PredicateNode]) extends PredicateNode {
        def predValue:Predicate= {
            ps.foldLeft(b.predValue)((acc, p) => p match {
                case "and"~x => LAnd(acc,x.predValue)
            })
        }
    }

    case class VariableNode(name: String) extends AtomNode {
        def atomValue: Atom = Variable(name)
    }

    case class StringLiteral(str: String) extends AtomNode {
        def atomValue: Atom = PurposeString(str)
    }

    case class IntegerLiteral(n: Int) extends AtomNode {
        def atomValue: Atom = PurposeInt(n)
    }

    case class FloatLiteral(f: Float) extends AtomNode {
        def atomValue: Atom = PurposeFloat(f)
    }

    case class BooleanLiteral(b: Boolean) extends AtomNode {
        def atomValue: Atom = PurposeBool(b)
    }
}

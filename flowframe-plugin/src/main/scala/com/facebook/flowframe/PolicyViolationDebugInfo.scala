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

/** Debugging information about why a policy constraint was violated. */
sealed abstract class PolicyViolationDebugInfo {
	val shortName: String
	val description: String
}

object BeginPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Begin Policy Violation"
	override val description: String =
		"""The policy of the receiver and control flow context combined (the access path policy) is more restrictive
		  |that the policy with which the method or lambda can be invoked (its begin policy).""".stripMargin
}

object ParameterPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Parameter Policy Violation"
	override val description: String =
		"The policy of an argument is more restrictive than the policy of its corresponding parameter."
}

object AssignmentPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Assignment Policy Violation"
	override val description: String = {
		"""The policy of an RHS expression is more restrictive than the policy of the LHS.
		  |Note that the policy of the control flow context lower bounds the RHS expression policy
		  |to prevent implicit flows.""".stripMargin
	}
}

object LambdaPCBeginPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Lambda PC Begin Policy Violation"
	override val description: String =
		"""The policy of the context (receiver and control flow) in which a lambda is constructed is more restrictive than
		  |the policy of the context in which the lambda can be invoked.""".stripMargin
}

object DefExprRHSPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Def definition Expression RHS Policy Violation"
	override val description: String =
		"The policy of the RHS is more restrictive than the policy of the `def` declaration."
}

object DefSigRHSPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Def definition Signature RHS Policy Violation"
	override val description: String =
		"The policy signature of the RHS is more restrictive than the policy signature of the `def` declaration."
}

object ValDefRHSPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Val definition RHS Policy Violation"
	override val description: String =
		"The policy of the RHS is more restrictive than the policy of the `def` declaration."
}

object AscribedPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Ascribed Policy Violation"
	override val description: String =
		"The policy of the expression policy is more restrictive than the policy of its type ascription."
}

object UnknownPolicyViolation extends PolicyViolationDebugInfo {
	override val shortName: String = "Unknown Policy Violation"
	override val description: String = "The policy constraint was violated for an unknown reason."
}

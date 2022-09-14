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

import com.facebook.flowframe.purpose.{PurposePolicyLang, PurposeParser}

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

/** Compiler plugin for checking information flow policies. */
class FlowFramePlugin(val global: Global) extends Plugin {
    val name = "flowframe"
    val description = "Check information flow control policies"

    var checkPolicies : CheckLabels[PurposePolicyLang] = new {
        val global: FlowFramePlugin.this.global.type = FlowFramePlugin.this.global
        override val runsAfter: List[String] = List("typer")
    } with CheckLabels[PurposePolicyLang] with PurposeParser

    val components: List[PluginComponent] = List[PluginComponent](checkPolicies)

    override val optionsHelp: Option[String] = Some(
        "  -P:flowframe:lang:<policy lang>     Enable policy checking for <policy lang> policies."
    )

    override def init(options: List[String], error: String => Unit): Boolean = {
        for (option <- options) {
            if (option.startsWith("lang:")) {
                option.substring("lang:".length) match {
                    case "purpose" => {
                        println("Enabling Purpose Policy checking")
                        checkPolicies.enable()
                        return true
                    }
                    case "fla" => error ("Policy language not supported.")
                    case "LH" => error ("Policy language not supported.")
                    case _ => error ("Policy language not supported.")
                }
            } else {
                error("Option not understood: "+option)
            }
        }
        false
    }
}

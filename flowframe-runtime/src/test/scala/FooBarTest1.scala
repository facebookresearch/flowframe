import com.facebook.flowframe.{FlowframeShouldFail, FlowframeShouldPass, Policy}
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

import org.apache.spark.sql._

@FlowframeShouldFail("insecure flow to constructor parameter")
object FooBarTest1 {
	def apply(spark:SparkSession): Unit = {
		import spark.implicits._
		val table = spark.createDataset(FooBarTable(spark).collect()) // any

		// flows-to violation from foobar.foo to BarTable.bar
		@Policy("fb::alice")
		val alice_val: Int = 42
		val bar = BarTable(alice_val + 20) // fb::bob

		bar match {
			case BarTable(_) => {}
			case _ if true => {}
		}
	}
}

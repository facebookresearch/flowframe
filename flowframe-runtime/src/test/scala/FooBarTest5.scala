import com.facebook.flowframe.{FlowframeShouldFail, FlowframeShouldPass}
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

@FlowframeShouldFail("insecure implicit flow from filter to parameter of argument of BarTable constructor")
object FooBarTest5 {
	def apply(spark:SparkSession): Unit = {
		import spark.implicits._
		val table = spark.createDataset(FooBarTable(spark).collect()) // any

		// flows-to violation from _.foo in foobarDF2.filter to begin policy of BarTable in realBarDF2.map
		val foobarDF2 = table.filter(_.foo > 1337)
		val barDF2 = foobarDF2.map { (r:FooBarTable) => 3 + r.foo }
		val realBarDF2 = barDF2.map(x => BarTable(x))
	}
}

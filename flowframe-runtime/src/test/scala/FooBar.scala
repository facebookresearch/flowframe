import com.facebook.flowframe.Policy
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

import org.apache.spark.sql.{Dataset, SparkSession}

//  https://www.internalfb.com/phabricator/paste/view/P409972081

@Policy("any") // table policy
case class FooBarTable(
	foo: Int @Policy("fb::alice"), // column policy
	bar: Int @Policy("fb::bob"),
	ds: String // default: @policy("any")
)

object FooBarTable {
	def apply(spark: SparkSession): Dataset[FooBarTable] = {
		import spark.implicits._
		spark.table("foo_bar").as[FooBarTable]
	}
}

@Policy("fb::bob")
case class BarTable(bar: Int @Policy("fb::bob"))

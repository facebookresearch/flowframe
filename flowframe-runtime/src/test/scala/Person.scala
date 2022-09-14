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

@Policy("any")
case class Person(
    name: String,
    phone: String @Policy("fb::auth")
)

object Person {
    def apply(spark: SparkSession): Dataset[Person] = {
        import spark.implicits._
        spark.table("foo_bar").as[Person]
    }
}

@Policy("fb::ads")
case class PersonAdInfo(
    name: String,
    topic: String
)

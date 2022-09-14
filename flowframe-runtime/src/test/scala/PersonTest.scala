import com.facebook.flowframe.{FlowframeShouldPass, FlowframeShouldFail}
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

import org.apache.spark.sql.SparkSession

@FlowframeShouldFail("insecure flow from filter to map")
object PersonTest {
    def areaCode(phone: String): String = {
        phone
    }

    def apply(spark:SparkSession): Unit = {
        import spark.implicits._

        // val mpkPeople = Person(spark).filter(person => person.name.contains("John"))
        val mpkPeople = Person(spark).filter(person => areaCode(person.phone) == "650")
        val golfAds = mpkPeople.map(person => PersonAdInfo(person.name, "golf"))
        golfAds.write.saveAsTable("golf")
    }
}

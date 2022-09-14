# FlowFrame: Ensuring Privacy of Spark Pipelines with Static Analysis

Internship Project, Summer 2022
Rolph Recto

This document outlines the design of the FlowFrame compiler plugin for Scala, which I worked on for my summer internship. If you are looking for a fast introduction to FlowFrame, read Owen Arden’s design document first: [Introducing FlowFrame](INTRO.md). This document attempts to be more thorough than Owen’s document and goes deeper into how FlowFrame is actually implemented. 

This document is structured as follows. First, we give some motivation for why we built FlowFrame. Next, we describes the design of the FlowFrame plugin. Finally, we describe a new API to Spark Datasets, which is necessary for properly tracking information flows in Spark queries.

## Motivation

FlowFrame enforces information flow control policies in Scala Spark applications, specified by policies that specify ["purpose limitations"](https://gdpr-info.eu/art-5-gdpr/).

FlowFrame is a Scala compiler plugin that performs *static, fine-grained information flow analysis* on code to detect privacy violations, in the spirit of the [Jif compiler](https://www.cs.cornell.edu/jif/). (Contrast this with the *dynamic, coarse-grained information flow analysis* implemented in, for instance, [LIO](https://hackage.haskell.org/package/lio).) If you are not familiar with language-based information flow control, see [the classic survey paper by Sabelfeld and Myers](https://www.cs.cornell.edu/andru/papers/jsac/sm-jsac03.pdf) for more details.

A core design goal for FlowFrame is to be as unintrusive as possible. This means minimizing annotation burden as much as it can while still being able to catch privacy violations. Unlike the Jif compiler, FlowFrame does not rely on a new Scala compiler front-end; instead it relies on [annotations](https://docs.scala-lang.org/tour/annotations.html), which is a standard, lightweight way to tag components in Scala programs with additional metadata. FlowFrame also relies on inference and sensible defaults (especially for policy signatures for methods) to minimize annotation burden.

## Example

Consider the following two tables: `Person`, which contains information about a person’s name and phone number; and `PersonAdInfo`, which contains information about what ad topics a person will be served. The `Policy` annotation on the `phone` field in `Person` dictates that it should only be used for purposes relating to authentication (e.g. two-factor authentication), while the policy annotation on the `PersonAdInfo` class dictates that the table it represents will be used for ads.

```
case class Person(
    name: String,
    phone: String @Policy("fb::auth")
)

@Policy("fb::ads")
case class PersonAdInfo(
    name: String,
    topic: String
)
```

Now consider the following Spark query, which takes the `Person` Dataset, filters only for rows with phone numbers with a Menlo Park area code, and then maps that into a `PersonAdInfo` Dataset.

```
def areaCode(phone: String): String = { ... }

def executeQuery(spark: SparkSession): Unit = {
    import spark.implicits._

    val mpkPeople = spark.getDataset[Person].filter(p => areaCode(p.phone) == "650")
    val golfAds = mpkPeople.map(person => PersonAdInfo(person.name, "golf"))
    golfAds.write.saveAsTable("golf")
}
```

FlowFrame will reject this query as insecure. First, it determines that the Dataset bound to the `mpkPeople` variable has a policy at least as restrictive as `fb::auth`, because of the filter. At the next line, however, the `mpkPeople` Dataset is mapped to a new Dataset that can be used for purposes other than authentication: specifically, the `PersonAdInfo` object created during the mapping operation can be used for ads.

Let’s dive into what actually happens when FlowFrame analyzes this code. FlowFrame performs its privacy analysis by generating a set of constraints that define how information flows through a program and then attempting to solve these constraints. If these constraints can’t be solved, then FlowFrame reports a privacy violation. Analyzing the code above, FlowFrame reports the following error:

```
Begin Policy Violation 
The policy of the receiver and control flow context combined (the access path policy) is more restrictive 
that the policy with which the method or lambda can be invoked (its begin policy). 
 
/Users/rolphrecto/projects/flowframe/library/src/test/scala/PersonTest.scala 
        12     
        13         
        14         val mpkPeople = Person(spark).filter(person => areaCode(person.phone) == "650") 
>>>>    15         val golfAds = mpkPeople.map(person => PersonAdInfo(person.name, "golf")) 
        16         golfAds.write.saveAsTable("golf") 
        17     } 
        18 }   
 
Constraint: 
{(((PersonAdInfo:204) and (any)) and (PersonTest.mpkPeople:202)) and (PersonTest.$anonfun$begin:203) <= fb:ads} 
Substitutions: 
PersonAdInfo:204 = any 
PersonTest.mpkPeople:202 = fb:auth 
PersonTest.$anonfun$begin:203 = any
```

At the bottom of the error message contains information about the policy constraint that failed to hold, as well as the intermediate solution that FlowFrame arrived at when the constraint failed. This usually helps diagnose the root of the reported privacy violation. Above the reported constraint is the part of the code associated with the constraint. This also might be helpful in determining the privacy violation.

(NB: Just because a constraint failed, it doesn’t mean that it is the “real” reason for the privacy violation. Some other constraint that didn’t fail might be the culprit! In general, determining which parts of a program is relevant to static analysis errors is an interesting research problem. See the [ShErrLoc paper,](https://www.cs.cornell.edu/andru/papers/diagnostic/popl14.pdf) for example.)

In the example, the failed constraint in the error message models the fact that information about the receiver of the `map` operation (`mpkPeople`) flows to the begin policy (see below) of the constructor for `PersonAdInfo`, which is `fb::ads`. That is, the context in which the

The intermediate solution reported states that the `mpkPeople` variable has a policy as restrictive as `fb::auth`, which surely does not flow to `fb::ads`---information to be used only for authentication should not be used for ads! This is the root of the privacy violation that FlowFrame reports.

(NB: The example in Owen’s doc is similar, but his explanation of why there is a privacy violation is slightly different. It’s a valid explanation, but I implemented FlowFrame to have equate the policy of a class with the begin policy of its constructors. The technical reasons for this additional constraint boils down to whether FlowFrame tries to infer the policy of fields without annotations or just assumes the information on those fields can flow anywhere. Right now, FlowFrame will try to do the former.)

## FlowFrame

FlowFrame consists of two main passes. First, a constraint generation phase traverses AST nodes and generates policy constraints that define the flow of information through code. A constraint solving phase then attempts to solve these constraints. The solver algorithm is adapted from the Jif compiler (which in turn is adapted from the algorithm by [Rehof and Mogensen](http://www.sciencedirect.com/science/article/pii/S0167642399000118).

The basic objects of FlowFrame’s analysis are **policies**, which represents classification of information into categories that have restrictions on how such information can be used. Mathematically, policies are elements of some [lattice](https://en.wikipedia.org/wiki/Lattice_(order)) ordered by a “flows-to” relation. If policy P1 flows to P2, that means P2 is at least as restrictive as P1. The bottom of the lattice is “public” information that can flow anywhere, and the top of the lattice is “secret” information that can only flow to other information tagged “secret.” A program is deemed insecure when FlowFrame generates a constraint that requires P1 to flow to P2 when it does not. FlowFrame’s constraint generator also uses the *join* operation (⊔) of lattices to describe policies that contain information from multiple sources. For example, the result of an operation containing inputs with policies tagged P1 and P2 will usually have policy P1 ⊔ P2. FlowFrame’s implementation is parametrized on the choice of what policy language to use, but the focus here is on policies.

FlowFrame uses policy annotations (written as `@Policy(p)`, where is a policy) written by developers to determine whether code is secure or not. There are four main places that FlowFrame checks for such annotations:

* **Classes**. Specifically, if case classes used as row types for Spark Datasets are annotated with a policy, the annotation represents a table policy. Class policies simultaneously provide (1) a lower bound on the information contained in objects of that class and (2) an upper bound on the context in which objects of that class can be created.
* **Fields**. Field policies on specific Dataset case classes act as column policies. Field policies act both as a lower bound on the information flowing out of the field and an upper bound on the information that flows to it.
* **Variable declarations**. Declarations can be annotated with a policy. Like field policies, these act as both an upper bound on information flow to and a lower bound on the information flowing out of the variable. If the declaration has a right-hand side, FlowFrame generates constraints to ensure the policy of the RHS is not more restrictive than the variable.
* **Methods**. Methods are associated with *policy signatures* (see below). The current implementation of FlowFrame does not have support for annotations for policy signatures, but it does have built-in support signatures for some Spark Dataset operations such as `map` and `filter`. 

While FlowFrame requires some amount of annotations to properly ensure privacy, it can infer most policies. When writing Spark queries, you can just annotate the case classes of input and output tables without annotating intermediate variables. 

FlowFrame performs its analysis by associating program components (expressions, classes, fields, method parameters, variables, and methods) with **policy structures**, which come in two variants: policy expressions (we sometimes abuse terminology just call these policies) and policy signatures. Most program components are associated with policy expressions, but methods and functions must be associated with policy signatures, which capture the following:

* the policy on the receiver of the method (the **access path policy**)
* the upper bound on the policy of the context the method/function can be called (the **begin policy**)
* the policies of the parameters
* the policy of the return value

To allow flexibility with handling methods/functions, FlowFrame allows associating method/function parameters with **abstract policies**. These represent policies that can differ at call sites, very much like generic types. Combined with **policy references**, which can refer to policies of arguments or receivers at call sites, these can allow expression of methods with **generic policies**. For example, a method with two parameters with abstract policies A1 and A2 and return policy A1 ⊔ A2 represents the fact that the return value contains information about the arguments. When called with arguments with policies `fb::auth` and `fb::ads` respectively, the policy of the return value becomes `fb::auth and fb::ads`. 

When FlowFrame encounters a method without a built-in signature, it will try to infer a signature very similar to this example, where the policy on the return value is the join of the policy of the receiver and the policies of all the arguments.

(NB: The implementation of generic policies in terms of abstract policies and policy references can be contrasted with introducing explicit label variables. I chose this design because it alleviates the possible annotation burden when writing policy signatures. In most cases only the return policy needs to refer to other policies at callsites anyway, so the return value can have a policy reference . Policy references prevents the developer from having to introduce a label variable just to express this dependency. Policy references also allow for expressing dependencies easily on policies deep within a policy structure, as is the case for higher-order functions like `map` and `filter` that take in lambdas as arguments.)

As mentioned before, FlowFrame captures the flow of information in programs by generating policy constraints. These constraints are largely standard for a fine-grained IFC analysis, with the exception of an “access path” context, which tracks the policy of the receiver at call-sites. You can think of it as an object-oriented extension of the standard PC label construct that prevents implicit leaks through control flow.

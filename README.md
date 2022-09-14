# FlowFrame

FlowFrame is a Scala compiler plugin that performs static, fine-grained
information flow analysis on code to detect privacy violations, in the spirit of
the Jif compiler (https://www.cs.cornell.edu/jif/).

The project is organized as a hierarchy of top-level traits, each of which
extend the functionality of the previous trait: CheckLabels extends
SparkSignatures extends DefaultPolicies extends AbstractPolicies extends Solvers
extends Constraints extends Policies.

Here is a quick rundown of what’s in each file:

- `purpose/PurposePolicies.scala` - implementation of Purpose Policies
- `purpose/PurposePolicyParser.scala` - parser for Purpose Policy annotations
- `purpose/predicates/Predicates.scala` - incomplete implementation of PurposePolicy predicates (i.e. the stuff that comes after the with operator in Purpose Policies)
- `CheckLabels.scala` - implements constraint generation pass
- `Constraints.scala` - data structures for policy constraints
- `DefaultPolicies.scala` - code for calculating default policies in the absence of policy annotations
- `FlowFramePlugin.scala` - implements Scala’s plugin interface
- `Policies.scala` - basic data structure for policies
- `AbstractPolicies.scala` - data structures for abstract policies, policy references, policy signatures and policy structures
- `PolicyViolationDebugInfo.scala` -helpful information for debugging policy violations
- `Solvers.scala` - policy constraint solver
- `SparkSignatures.scala` - policy signatures for Spark operations

## Requirements
FlowFrame currently supports Scala 2.11 and 2.12.

## Building FlowFrame
Run `sbt package` to create jars for the compiler plugin and the runtime. You can 
find these jars in `flowframe-plugin/target` and `flowframe-runtime/target`.

## Using FlowFrame

To use FlowFrame, add the following commandline options to `scalac`:
- `-Xplugin:</path/to/plugin/jar>`
- `-P:flowframe:lang:<policy language>`

Currently, FlowFrame only supports the `purpose` policy language.

## How FlowFrame works
FlowFrame uses @policy annotations on case classes representing database table
rows accessed through the Spark Dataset API. Any named entity can be given explicit
policy annotations, but FlowFrame attempts to infer annotations whenever possible.
Ideally, only input and output Datasets will require annotations.

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License
FlowFrame is Apache 2.0 licensed, as found in the LICENSE file.

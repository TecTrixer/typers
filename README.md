# TypeRS

This is a tiny Mini-Haskell type constraint solver.

## Installation

You can either clone the repository and build it yourself or you can use cargo directly (recommended):

```sh
cargo install --git https://github.com/TecTrixer/typers
```

## Usage

You can load the type constraints from a file by just specifying the file name:

```sh
typers constraints.txt
```

Alternatively, you can enter the constraints into `stdin`. Note that you have press `Ctrl+D` / `^D` to mark the end of your input:

```sh
typers
t0 = t1 -> Bool
t1 = (Int, t1)
^D
```

If the variable you want to solve for is not `t0` but instead `tX`, you need to give this information to the solver with the `-g / --goal-var` flag:

```sh
typers -g X constraints.txt
```

## Type Constraint Grammar

In general you are should be able to use most basic syntax for Mini-Haskell:

- Tuples: `t0 = (Int, Bool)`
- Functions: `t0 = Int -> Bool`
- Variables: `t0 = t1 -> t2`
- Int's and Bool's: `t0 = (t1, Int) -> Bool`

Nested constraints are possible as well:

`t0 = (Int -> t1) -> (Bool, (Int -> t1) -> t2)`

Variables should always have the form `tX` where `X` can be any number (`t0, t4, t10, t1234` are all valid).

There are single line comments which begin with `//`, there are no multiline comments yet.

## Algorithm

To solve the system of type constraints there are three phases:

### Accumulating constraints

As long as there are multiply type constraints with the same left hand side variable, the solver will greedily match them, add any new encountered constraints and remove one of the original constraints:

```
t1 = t2 -> t3
t1 = Int -> (t4, t5)
```

After combining these rules, the following rules are left:

```
t1 = t2 -> t3
t2 = Int
t3 = (t4, t5)
```

Furthermore, whenever a simple rule is encountered (a rule of the form `tX = tY`), one of the variables is being replaced with the other one:

```
t0 = t1 -> t2
t2 = t1
```

The above system will be reduced to:

```
t0 = t1 -> t1
```

### Cycle Detection

In this stage the solver uses topological sorting to find out whether there exists a cycle in the type constraints. This will lead to infinite types and therefore the original function was not typeable in this case.

To construct the directed graph, all variables are nodes and there exists an edge from `tX` to `tY` if there is a rule with `tX` on the left hand side and `tY` on the right hand side. Then a simple breadth first search is enough to detect whether a cycle exists.

### Substituting contraints

In the last stage, the solver selects the "goal rule" which by default is the type constraint where `t0` is on the left hand side. Then it substitutes all variables in the right hand side of this rule greedily. Every encountered variable which has a corresponding rule will be replaced. This process goes on until nothing can be replaced anymore, which will then be the most general type.

## Mini-Haskell

The variant of Mini-Haskell the constraint solver uses is from the course `Formal Methods and Functional Programming`, which is being taught at ETH Zürich.
Mini-Haskell is a tiny subset of Haskell used to teach typing in the lambda-calculus. It has the following structures: variables, ints, bools, functions (and function application) and tuples.

## Writing a SAT Compiler
### & Making Sure It Works

Here's the goal: we're going to write a compiler to change high-level logical constraints to SAT.

# Intro & Background
#### What's SAT? *** verify against wiki

SAT is short-hand for "satisfiability", in particular **boolean satisfiability**. Boolean satisfiability is a classical problem in computer science which asks if we can determine whether a given boolean equation (like "A or B") has an assignment (like "A = true, B = false") which makes the entire equation true (thereby "satisfying it").

Some equations are not satisfiable, like "A and not A".

A boolean equation is made up of boolean variables tied together with ANDs, ORs and NOTs. In theory it is NP-hard to determine whether equations are satisfiable. In practice SAT solvers work very well for many extremely interesting real world problems.

Most SAT solvers interface use a format called DIMACS CNF. DIMACS is a more general format, and CNF stands for "conjunctive normal form", which means that the boolean equation is an "AND of ORs". An example is "(A or B) and (not C or B or D) and ...". Any boolean equation can be canonicalized in the CNF format so we don't lose generality by talking about how to translate logical expressions to general boolean equations.

I'm using a SAT solver as a black box solver, so we don't need to know any details about how they work (though they're very interesting!) We just need to know how to put expressions in (CNF) and interpret the expressions we get out.

The goal is to write a program that will compile some human-readable representation of a logical expression to DIMACS CNF. We'll look at the syntactic details of the source & target formats later. First let's talk about how we'll translate from a subset of first order logic called "cardinality constraints" to boolean equations. At the end we'll talk about how to use cardinality constraints to represent any first-order equation.


#### Cardinality Constraints

Before we talk about cardinality constraints, let's set up a motivating running example. Let's say we've got 4 balls lined up on a table: [ ball1, ball2, ball3, ball4 ]. Each ball can be either red or blue, and we want to find a way to have 2 red and 2 blue balls (like [red, blue, red, blue] is a solution).

This encodes naturally in binary- we'll say that the four balls are boolean variables A, B, C, D and that if a variable is assigned "true" then it is red, and if it assigned "false" then it is blue. This means we'll write [red, blue, red, blue] as "A and (not B) and C and (not D)". When we say that we want 2 reds and 2 blues is the same as saying we want 2 variables to be true and 2 variables to be false.

We'll briefly mention about how to encode arbitrary (more complicated) data at the end but it's not much more difficult.

The brute force solution is to write down all the ways our constraint could be true ("[r, r, b, b] OR [b, b, r, r], OR ect"). Unfortunately this is exponential in the number of orderings, so this is computational infeasible for even relatively short sequences.

Instead we'll write down the logic for counting the number of trues and make sure that number is 2 using a technique that I think is absolutely ingenious- we'll write down the logic for an **adder circuit** to count the number of trues! Actually, we'll be implementing the popcount algorithm ("population count") which counts the number of set bits in a boolean string.

The really cool part of this technique is its efficiency! It allows us to mash these cardinality constraints through a SAT solver using only a linear number of variables & constraints.

# Algorithm
#### Popcount

Okay, so first we should talk about popcount, but the example on wikipedia is really good so you should just go read that and then come back.

Basically you've got a boolean string with n numbers, and you're going to split that in half into 2 strings with n/2 numbers. In the wiki example they make a boolean mask by bitshifting to select every other number, but the way you split the string doesn't actually matter. Then, you'll add those 2 n/2 numbers (using a n/2 sized adder!) resulting in an n/2 sum. Then repeat on the sum- divide into 2 n/4 length strings and add! Continue this until you have a total cumulative sum. Perfect.

#### Half Adders, Full Adders & Ripple Carry (oh my!)
**Half Adders**
Okay, so let's take a moment to talk about how adder circuits work. Let's start simple: a half-adder takes two inputs A & B, and adds them, producing a sum S and a carry C. Adders are often represented with diagrams because they correspond to actual physical gates in hardware, but we just care about the logic. We can also confirm that the equations for the output bits are true by consulting a truth table:

Diagrams & Equation & Truth Table

**Full Adders**
Diagrams & Equation & Truth Table

**Ripple Carry Adders**
Diagrams & Equation & Truth Table

#### Pop Countin' with a Ripple Carry Adder
Ok

#### From Circuit to Solver


# Generalizing to Arbitrary First-Order Logic

#### More complicated data objects

#### Arbitrary first order logic

# Syntax
I've included this section detailing the syntax of the input & output formats for completeness, but it isn't the interesting part. You can checkout the full code listing (including more examples) on github.

#### Architecture & Representation
Once more, the goal is to compile from a human-readable representation of first-order logic constraints to CNF (and back again!) We split our pipeline into 3 natural sections: the front-end takes our input target to a comfortable internal representation (IR), the middle-end changes the IR to cast the first-order logic as cardinality constraints, the back-end emits the corresponding DIMACS CNF.

#### Front-End

The input format is whatever I want it to be. I tailored it to the domain I'm working in, but if I wanted to work in a different domain, all I would need to do is update the parser to understand the new input format. Here is what our running example looks like in my input format:

```
Feature: Color
   Level: Red
   Level: Blue
Experiment:
   4 x Fully_Cross(Color)
Constraints:
  #Red=2
  #Blue=2
```


#### Middle-End
Not here yet.

#### Back-End


# Verification

# Lit Review
Here's a list of relevant links:

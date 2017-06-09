
# .test specification:

3 modes, specifies with command line flags


| Flag       | Arguments                                             | Example  |
| -------    |:----------------------------------------------------: | :-------:|
| exhaustive | constraint name, object set name                      | ./test exhaustive fewer-than-k-in-a-row simple-object |
| random     | # constraint draws, # argument draws, object set name | ./test random 10 10 simple-object |
| on demand  | specification file path, object set name              | ./test on-demand specification-file simple-object |
| help       |                                                       | ./test help    |



## How the tester works:
1. Generate specification files by calling the compiler with the specify flag & whatever arguments we got in
2. For every \*.spec file in the `tests` directory, calls the compiler with the compile flag
3. Runs all \*.cnf files in the `tests` directory through the SAT solver, piping output into \*.result files
4. For every \*.result file in the `tests` directory, calls the compiler with the validate flag, and passing in the corresponding \*.spec file.



----------------------------------------------------------------------------

# compiler pipeline specifications:

## specify
- flag: exhaustive or random
	- exhaustive: constraint name & object set file path
		- object set : this is a copy & paste
		- constraint name: match to get figure out arguments
		- need to parse object set to get search space for other arguments.
			- --> generate all valid argument function
		- generate 0.spec files
	- random: # constraint draws,  # argument draws, object set file path
		- object set : this is a copy & paste
			- parse so that levels are available
		- for # constraint draws
			- random number: # of constraints
			- grab that # of constraints
			- for each constraint grab #arguments arguments
				- --> generate all valid arguments function
				- --> also pick levels at random
		- generate 0.spec files

Q: what % random constraints combos are unsatisfiable?

## parse
- takes \*.spec files
- parses into:
  - dictionary of names to numbers
  - number levels total
  - total number of objects
  - constraint representation DS, list of
	- name :: String
	- compiler function to call (need to make a union type)
	- validation function to call (need to make a union type)
	- NOTE: n .spec constraints may compile to MORE than n actual constraints


## compile
- testing flag: if we're testing then don't run validator's contradiction search, otherwise do and bail early on failure
- function for every kind of supported constraint, produces a CNF

## codegen
- all functions that take CNFs and make DIMACS strings

## validate
- verbose flag: whether or not to print successes
- parse .spec file
- validate whether the specification is satisfiable!
- use the spec file to match on which validation functions to call
	- call those functions with contents of result file

## TODO: translate result to high level

## driver : handles all IO
- specify
	- inputs: arguments from .test
	- --> parse, spec, file gen
	- outputs: n \*.spec files
- compile
	- inputs: spec file, flag about whether we're in testing mode
	- --> parse, compile, code gen
	- outputs: 1 \*.cnf file
- validate
	- inputs: spec file + result file, flag about verbosity
	- --> parse, validate
	- outputs:
		- writes failed files (.spec & .cnf) to `failedTests` directory
		- also writes failed results to stdout

----------------------------------------------------------------------------

# JSON spec

- **objects** dictionary
	- feature names  ->  list of level names
	- String -> [String]
- **experiment** object
	1. full-crossed: list of which levels to fully cross
		- all other levels are glommed on (todo: how?)
	2. number reps: # of times each fully crossed set is repeated
- **constraints** list of dictionaries
	- each constraint has:
		1. "constraint" with string name of constraint
		2. all other arguments -> names & values
			- this could be a list of dictionaries if that's easier to parse
			- need to specify level & factor because level names aren't guaranteed to be unique across factors

--------------------------------------------------------------------

# Object Sets

Four object sets available for testing. Each of these is fully-crossed, and each one has 5 instances of each possible object.

1. The simplest possible objects: Each has a single feature "color" with 2 levels. There are 2*5 = 10 objects in this set.
	- `red`
	- `blue`

2. Objects with more values: Each has a single features "color" with 3 levels. There are 3*5 = 15 objects in this set.
	- `red`
	- `blue`
	- `green`

3. Objects with more fields: Each has two features "color" and "shape" each with two levels. There are 4*5 = 20 objects in this set.
	- `red square`
	- `blue square`
	- `red circle`
	- `blue circle`

4. A complicated object set: Each has 3 features "color", "shape", "saturation", each with 2 or 3 levels: 5*12 = 60 objects in this set.
	- `dark red circle`
	- `light red circle`
	- `dark red square`
	- `light red square`
	- `dark red triangle`
	- `light red triangle`
	- `dark blue circle`
	- `light blue circle`
	- `dark blue square`
	- `light blue square`
	- `dark blue triangle`
	- `light blue triangle`
5. Same as #4 but with 20 instances instead of 5, for a total of 12*20 = 240 objects

	----------------------------------------------------------------------------
# .test-all specification:

This is a wrapper that runs lots of relevant tests. Modes set by command line args

## Default (no args)
- Exhaustive Tests
  - For every constraint
    - For every object test set (except #5)
- Random Tests, test set #5
  - 50 constraint draws
    - 50 argument draws

## Random nTimes
- Runs the "random section" (with parameters `50 50`) nTimes
	- Can let it run for a weekend ad absurdum

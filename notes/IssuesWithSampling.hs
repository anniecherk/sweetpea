
module Main where

import FrontEnd
import CodeGen
import DataStructures



-- at least 2 red circle every 5

-- m of n
-- at least {2 out of these 3(red, blue, green)} every 5 {red, blue, black, white}
-- sample with and without replacement
-- at least 3 (sample w/out replace (red, blue, green)) every 5 <= make sure 4 of 3 of 5 unsat
-- allOf / anyOf
-- red blue black white black
-- red red black white black

-- Agenda
-- 1. "sets" with sample w/ & w/out replacement
-- 2. unsat & what to do about it
-- 3. multiFullyCrossedBlocks
-- 4. transition constraint plans
-- 5. NSF GRFP / other applications / OBT workshop paper / extensibility
-- 6. SAT multiple solutions & max undefined vars
-- 7. an efficient encoding of there exists

-- window constraints: strided & sliding windows || folding window is like miniblocks?
-- same kinds of constraints that go onto the blocks that go onto the windows
-- I want to see any of these elements in the window
-- I want to see all of these elements
-- fully cross ~~ sample w.out replacement

-- what's the usecase for all of? ==> fully crossed


main :: IO ()
main = do let color = NTNode "color" [LeafNode "red",   LeafNode "yellow",
                                      LeafNode "green", LeafNode "blue"]
          let shape = NTNode "shape" [LeafNode "circle", LeafNode "square"]
          let design = [color, shape]
          let numTrials = (fullyCrossSize design)


-- red red blue blue :at least 2 of 4
-- red yellow blue blue: at least 2 of 4
--
-- with replacement: I want a total of 3 of anything in the set
-- red, red, red, red
-- without replacement> I want a total of (3 every 4) but only drawing each element once
-- red red gren blue
--
-- w/out rep:
-- k every j and k < size set UNSAT
-- k === size of set
-- I want to see every member of this set every j elements
-- if j < size of set also UNSAT
--
-- sample w. rep => but actually sampling from the set of all trials
--
--
--
-- I want 2 of dark colors w/out replacement
-- red green red green



          let warmColors = SampleWithReplacement [["color", "red"], ["color", "yellow"]]
          let darkColors = SampleWithOutReplacement [["color", "red"], ["color", "green"], ["color", "blue"]]
          let allColors  = SampleWithReplacement [["color", "red"], ["color", "yellow"], ["color", "green"], ["color", "blue"]]


          let constraints = [AtLeastKeveryJ 2 4 ["color", "red"], FullyCross]
          let constraints2 = [AtLeastKeveryJ 2 4 ["color", "red"]]



-- This is an experiment with a single fully-crossed block
-- This means there are 8 trials:
--     2: reds, yellows, greens, blues,
--     4: circles, squares

-- Examples of constraints that are UNSAT

-- for all windows size 2, at least 2 elements are red
-- for all windows of size 2, there exists at least one window that has 2 red in a row
-- ==> transition constraints

-- no more than 7 task reps in a row ==> suggest no more than 5 task reps in a row

-- AtLeastKInARow 2 [["color", "red"]]
-- *okay* AtLeastKInARow 4 allColors => counter-example: rygbbgyr
--     the first one is wrong because "at least k in a row" means FOR ALL windows -- maybe it should mean "there exists?"
--     the second one is OKAY- it means, out of the 8 make sure the first 4 & last 4 each have all the colors
--        this is a different mechanism then "blocks" because it still allows the circle/squares to be distributed however

-- AtLeastKEveryJ k 9 [["color", "red"]]
--     it's not exactly incorrect but suspicious if   j > (length block)
--     we are in trouble if  k > j

-- AtLeastKEveryJ 3 j [["color", "red"]]
--     this block only has 2 reds available, so options for valid k's = {0, 1, 2}

-- AtLeastKEveryJ 3 4 warmColors
-- *okay* AtLeastKEveryJ 2 4 warmColors
--     when we deal with sets we have to make sure that

-- ExactlyKInARow k [["color", "red"]]
-- *okay* ExactlyKEveryJ 2 8 [["color", "red"]]
--      we're in *big* trouble using exactlyKInARow constraint with a fully crossed block
--      we're *only* okay using "exactly k every j" if we choose k & j so that they scale up to the # of that type of trial we have / block

-- AtMostEtc has the same kinds of troubles as AtLeastEtc


--     Some solutions to the interface problem:
-- Are there better names I could be using?
-- Program repair: You say "I want atmost 1 out of 8 reds"; program says... did you mean: 1 of 4, or 2 of 8?
-- Interactive repl: Ask: "I want atmost j k reds", repl replies "valid options are (1, 4), (2, 8), ..."
-- Interactive (rather than purely textual) interface: drop down valid options




          let block = makeBlock numTrials design constraints
          let ast = [block]
          let (nVars, cnf) = runExperiment ast
          putStrLn $ showDIMACS cnf nVars

-- transitions

-- # block combinators:
-- #  sequence combinators:
-- #    sequence / randomWithReplacement / permutations
-- #    repeat
-- #  mix
-- #  optional # at random add this block, or don't    # null blocks
--
-- # block filler : what's in it?
-- #  sample / fully cross
--
-- # block builder :
-- #  exactly






-- SMALLER EXAMPLE
          -- let color = NTNode "color" [LeafNode "red", LeafNode "blue"]
          --           let design = [color]
          --           let block = makeBlock (fullyCrossSize design) [color] [FullyCross]
          --           let ast = [block]
          --           let (nVars, cnf) = runExperiment ast
          --           putStrLn $ showDIMACS cnf nVars

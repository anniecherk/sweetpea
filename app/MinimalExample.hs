module MinimalExample where

-- This afternoon
-- 1. How do we constrain fully crossed?
-- 2. Set up minimal example
--   a. Minimal block rewrite
--   b. Does it desugar correctly?
--   c. Write tests & update the error handling
-- 3. Deal with blocks for real
-- 4. Deal with exact constraint syntax/wording & wtf is going on with transitions


-- 1. Import that fixes all the errors

import ParserDS


-------- Experiment ---------
main :: IO ()
main = experiment $ RBlock (fullycross design) [noConstraints]
       where
         ----------- Streams --------------
        color  = Label "color" ["red", "blue"]
        shape  = Label "shape" ["circle", "square"]

        --------- Transitions --------------

        ---------- Constraints -------------

      --  theConstraints = Constraints (count (None, 3) color)     -- this count syntax doesn't fly

        -- syntax/constructor for "no constraints"
        noConstraints = NoConstraints

        ---------- Design ------------

        design = cross [color, shape]

        ----------- Blocks ------------    # fully crossed constraints not good enough!!!

module Main where

import StatefulCompiler
import Testers
import System.Environment
import System.Directory
import Data.Char
import Data.List.Split


splitOnArgs :: [String] -> IO ()
splitOnArgs args
--------------------------------------------------------------------------------
  --(they have hard-coded sol's: I checked these by hand early on)
  -- useage: ./run_tests.sh halfAdder
  | head args == "halfAdder"
  = mapM_ (\(i, x) -> writeFile ("generated_tests/halfAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testHalfAdderDIMACS

  ------------------------------------------------------------------------------------------------------------------------
  -- meant to be run from the run_tests.sh script: it generates all the files, and then diffs them against the SAT result
  -- useage: ./run_tests.sh fullAdder
    | head args == "fullAdder" --tests ALL inputs to the full adder
    = do                                                                    -- zipping index for file names
      mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testFullAdderDIMACS
      mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".sol") x) $ zip [0..] solnFullAdder

  ------------------------------------------------------------------------------------------------------------------------
  -- meant to be run from the run_tests.sh script: it generates all the files, and then diffs them against the SAT result
  -- useage: ./rippleCarry.sh --ripplecarry 6 // where 6 specifies the length of the sequenece we're exhaustively testing
  -- defaults to size 2.. not sure if this is the best design but...
    -- | head args == "rippleCarry" --tests ALL inputs to the full adder
    -- = do
    --   let rippleSize = if length args == 2
    --                    then read (head (tail args)) :: Int
    --                    else 2                                                                     -- zipping index for file names
    --   mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
    --   mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize


  ------------------------------------------------------------------------------------------------------------------------
    | otherwise = putStrLn $ "I don't know what you want from me! \n You said: " ++
    show args ++ "\nTry one of these cmdln args:\n\
    \generatePopCount [2], testPopCount [v], \n\
    \generateKofN [2], testKofN [v], \nhalfAdder, fullAdder, rippleCarry [2] \n"


--------------------------------------------------------------------------------
  | otherwise = putStrLn $ "I don't know what you want from me! \n \
  \You said: " ++ show args ++
  "\nTry one of these cmdln args:\n\
  \generatePopCount [2], testPopCount [v], \n\
  \generateKofN [2], testKofN [v], \n \
  \halfAdder, fullAdder, rippleCarry [2] \n"



main :: IO ()
main = do
        args <- getArgs
        if length args < 1
        then putStrLn "use commandline arg <generate n> to generate popCount tests of strings of length n \n or  commandline arg <test> to test all files in popCountResults directory."
        else splitOnArgs args

module Main (main) where

-- import Src
-- import Parser
-- import TypeCheck
-- import Desugar
-- import Core
-- import Simplify
-- import Translations

-- REPL parameterised by the type of its state
type REPLLoop s = (String, s) -> IO (String, Prompt, s)

data REPL s = REPL { replInit :: IO s
                   , replLoop :: REPLLoop s }

type Prompt = String

runREPL :: REPL s -> IO ()
runREPL repl
  = do init_state <- replInit repl
       runREPLLoop (replLoop repl) init_state

runREPLLoop :: REPLLoop s -> s -> IO ()
runREPLLoop repl_loop state
  = do input <- getLine
       (output, new_prompt, new_state) <- repl_loop (input, state)
       putStrLn output
       putStr   new_prompt
       runREPLLoop repl_loop new_state

adder :: REPL Int
adder = REPL
  { replInit = do { putStr "adder> "; return 0 }
  , replLoop = \(input, state) ->
               do let new_state = state + read input
                  return (show new_state, "adder> ", new_state)
  }

main :: IO ()
main = runREPL adder
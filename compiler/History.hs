module History where

type Hist = [String]

empty :: Hist
empty = []

insert :: Hist -> String -> Hist 
insert xs cmd = cmd : xs



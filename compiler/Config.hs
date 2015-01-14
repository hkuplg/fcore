module Config where

data IntersectionBias = LeftBiased | RightBiased

-- If a record contains more than one field with the same label, the first field
-- for the label is preferred. For example, `{ age = 3, age = 8 }.age` evaluates
-- to 3. This configuration should be consistent with the corresponding one in
-- the simplifier.
intersectionBias :: IntersectionBias
intersectionBias = LeftBiased

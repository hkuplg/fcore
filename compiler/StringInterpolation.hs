module StringInterpolation
  ( expand
  ) where

import Src

type InterpolatedString = [Component]

data Component = Literal String | Placeholder String

interpolate :: String -> ReaderExpr
interpolate = expand . parse

parse :: String -> InterpolatedString
parse = undefined

expand :: InterpolatedString -> ReaderExpr
expand = undefined

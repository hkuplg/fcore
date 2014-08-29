module DeInter where

import qualified SystemFI.Syntax as F
import SystemFI.TypeCheck (transExpr)

import Unsafe.Coerce
import Control.Monad.State

deInter :: F.Expr t e -> F.Expr t e
deInter e = unsafeCoerce $ snd (evalState (transExpr (unsafeCoerce e)) 0)
module DeInter where

import qualified SystemF.Syntax as F
import qualified SystemFI.Syntax as FI
import SystemFI.TypeCheck (translate)

import Unsafe.Coerce
import Control.Monad.State

deInter :: FI.Expr t e -> F.Expr t e
deInter e = unsafeCoerce $ snd (evalState (translate (unsafeCoerce e)) 0)
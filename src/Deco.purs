module Deco where

import Control.Monad.Eff (Eff)
import DOM (DOM)

import Data.Unit (Unit)

foreign import setBackground
  :: forall eff
  .  Int
  -> Eff (dom :: DOM | eff) Unit

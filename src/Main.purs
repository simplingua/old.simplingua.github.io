module Main where

import Control.Monad.Aff (launchAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import DOM (DOM)
import Deco (setBackground)
import Dictionary (readDictionary)
import GUI (mainGui)
import Prelude (Unit, bind, void, ($), (>>=))

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, dom :: DOM, random :: RANDOM | e) Unit
main = void $ launchAff do
  d <- readDictionary "simplingua.xlsx"
  _ <- liftEff' $ mainGui d
  liftEff' $ randomInt 0 150 >>= setBackground

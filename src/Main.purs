module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, text, button, span, div)
import Pux.Html.Events (onClick)

import Types

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

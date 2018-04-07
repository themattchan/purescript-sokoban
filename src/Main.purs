module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector, QuerySelector(..)) as DOM
import Data.Maybe
import Partial.Unsafe (unsafePartial)
import React (ReactComponent)
import React (createFactory) as R
import React.DOM (text, button, p') as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T

import Types

main :: forall e. Eff (dom :: DOM | e) (Maybe ReactComponent)
main = do pure Nothing
--  log "Hello sailor!"

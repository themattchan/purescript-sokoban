module Test.Main where
import Control.Monad.Eff.Exception
import Data.Record.ShowRecord
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Node.FS.Sync
import Node.Encoding
import Node.FS
import Everything

main :: forall e. Eff ( console :: CONSOLE
                      , exception :: EXCEPTION
                      , fs :: FS
                      | e
                      ) Unit
main = do
  input <- readTextFile UTF8 "data/100Boxes.txt"
  log $ show $ map showRecord $ parseBoards input
--  log "You should add some tests."

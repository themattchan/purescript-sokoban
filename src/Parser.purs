module Parser where

import Prelude
import Data.Array as A
import Data.String as S
import Data.Ord.Max (Max(..)
import Data.Matrix as Matrix
import Data.TraversableWithIndex
import Data.Functor.Compose
import Data.Tuple
import Types

-- Wall                     #   0x23
-- Player                   @   0x40
-- Player on goal square    +   0x2b
-- Box                      $   0x24
-- Box on goal square       *   0x2a
-- Goal square              .   0x2e
-- Floor                (Space) 0x20

parseBoards :: String -> List State
parseBoards = mapMaybe parseBoard <<< S.split (Pattern "\n\n")

parseBoard :: String -> Maybe State
parseBoard = makeBoard
           <<< A.span (any ((_ == ';') <<< _.head) <<< S.uncons)
           <<< S.split (Pattern "\n")
  where
    makeBoard {init, rest} = do
        Tuple (MP {player:maybePlayer, boxes: maybeBoxes}) board
           <- unwrap (doCells init)
        player <- maybePlayer
        boxes <- maybeBoxes
        pure $ {levelName, player, boxes, board}
      where
        levelName = S.drop 2 <$> A.head rest
        padR n xs = xs <> A.replicate (n - A.length xs) Empty
        width = unwrap $ foldMap (Max <<< A.length) init
        doCells = map Matrix.fromArray <<<
                  traverseWithIndex
                    (\i ->
                      map (padR width) <<<
                      traverseWithIndex (\j -> convert {x:j, y:i}) <<<
                      S.toCharArray
                    )


newtype MovablePieces = MP {player :: Maybe Player, boxes :: Maybe Boxes}
derive instance newtypeMovablePieces :: Newtype MovablePieces _

instance monoidMovablePieces :: Monoid MovablePieces where
  mempty = MP {player:Nothing, boxes:Nothing}
  mappend (MP {player:p1, boxes:b1}) (MP {player:p2, boxes:b2})
    = MP {player:p1<|>p2, boxes:b1<>b2}

convert :: Coord -> Char -> Compose Maybe (Tuple MovablePieces) Cell
convert c '#' = Compose $ Just $ Tuple mempty Wall
convert c '@' = Compose $ Just $ Tuple (mempty{player=Just c}) Empty -- emit player
convert c '+' = Compose $ Just $ Tuple (mempty{player=Just c}) Goal -- emit player
convert c '$' = Compose $ Just $ Tuple (mempty{boxes=Just (pure c)}) Empty -- emit box
convert c '*' = Compose $ Just $ Tuple (mempty{player=Just (pure c)}) Goal -- emit box
convert c '.' = Compose $ Just $ Tuple mempty Goal
convert c ' ' = Compose $ Just $ Tuple mempty Empty
convert _ _   = Compose Nothing

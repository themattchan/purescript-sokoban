module Parser where

import Types

import Prelude
import Control.Alternative ((<|>))
import Data.Array as A
import Data.String as S
import Data.Ord.Max (Max(..))
import Matrix (Matrix)
import Matrix as Matrix
import Data.Foldable
import Data.Traversable
import Data.TraversableWithIndex
import Data.Functor.Compose
import Data.Profunctor
import Data.Tuple
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty as NE

-- Wall                     #   0x23
-- Player                   @   0x40
-- Player on goal square    +   0x2b
-- Box                      $   0x24
-- Box on goal square       *   0x2a
-- Goal square              .   0x2e
-- Floor                (Space) 0x20

parseBoards :: String -> Array Level
parseBoards = A.mapMaybe parseBoard <<< S.split (S.Pattern "\n\n")

parseBoard :: String -> Maybe Level
parseBoard = makeBoard
           <<< A.span (any ((_ == ';') <<< _.head) <<< S.uncons)
           <<< S.split (S.Pattern "\n")
  where
    makeBoard :: {init :: Array String, rest :: Array String } -> Maybe Level
    makeBoard {init, rest} = do
        Tuple (MP {player:maybePlayer, boxes: maybeBoxes}) board
           <- doCells init
        player <- maybePlayer
        boxes <- maybeBoxes
        pure $ {levelName, player, boxes, board}
      where
        levelName = S.drop 2 <$> A.head rest
        padR n xs = xs <> A.replicate (n - A.length xs) Empty
        width = unwrap $ foldMap (Max <<< S.length) init
        -- TODO use Data.Distributive.collect here
        doCells :: Array String -> Maybe (Tuple MovablePieces (Matrix Cell))
        doCells = traverse Matrix.fromArray <=< unwrap <<<
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
instance semigroupMovablePieces :: Semigroup MovablePieces where
  append (MP {player:p1, boxes:b1}) (MP {player:p2, boxes:b2})
    = MP {player:p1<|>p2, boxes:b1<>b2}

convert :: Coord -> Char -> Compose Maybe (Tuple MovablePieces) Cell
convert c '#' = Compose $ Just $ Tuple mempty Wall
convert c '@' = Compose $ Just $ Tuple ((\(MP x) -> MP $ x{player=Just $ Player c}) mempty) Empty -- emit player
convert c '+' = Compose $ Just $ Tuple ((\(MP x) -> MP $ x{player=Just $ Player c}) mempty) Goal -- emit player
convert c '$' = Compose $ Just $ Tuple ((\(MP x) -> MP $ x{boxes=Just $ NE.singleton $ Box c}) mempty) Empty -- emit box
convert c '*' = Compose $ Just $ Tuple ((\(MP x) -> MP $ x{boxes=Just $ NE.singleton $ Box c}) mempty) Goal -- emit box
convert c '.' = Compose $ Just $ Tuple mempty Goal
convert c ' ' = Compose $ Just $ Tuple mempty Empty
convert _ _   = Compose Nothing

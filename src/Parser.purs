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
import Data.Maybe.First
import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty as NE
import Effect.Console (log)
import Effect.Unsafe

import Data.Record.ShowRecord


-- Wall                     #   0x23
-- Player                   @   0x40
-- Player on goal square    +   0x2b
-- Box                      $   0x24
-- Box on goal square       *   0x2a
-- Goal square              .   0x2e
-- Floor                (Space) 0x20

trace :: forall a. String -> a -> a
trace m a = unsafePerformEff (log m >>= \_ -> pure a)

traceShow :: forall a. Show a => a -> a
traceShow a = trace (show a) a

parseBoards :: String -> Array Level
parseBoards = A.mapMaybe parseBoard <<< S.split (S.Pattern "\n\n")

parseBoard :: String -> Maybe Level
parseBoard = -- (flip trace <*> (show <<< map showRecord)) <<<
             makeBoard
           <<< A.span (any ((_ /= ';') <<< _.head) <<< S.uncons)
           <<< S.split (S.Pattern "\n")
  where
    makeBoard :: {init :: Array String, rest :: Array String } -> Maybe Level
    makeBoard {init, rest} = do
        Tuple (Tuple maybePlayer maybeBoxes) board
           <- doCells init
        player <- unwrap maybePlayer
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
                      traverseWithIndex (\j -> Compose <<< charToCell (coord j i)) <<<
                      S.toCharArray
                    )

type MovablePieces = Tuple (First Player) (Maybe Boxes)

charToCell :: Coord -> Char -> Maybe (Tuple MovablePieces Cell)
charToCell c '#' = Just $ Tuple mempty Wall
charToCell c '@' = Just $ Tuple (Tuple (pure (Player c)) mempty) Empty -- emit player
charToCell c '+' = Just $ Tuple (Tuple (pure (Player c)) mempty) Goal -- emit player
charToCell c '$' = Just $ Tuple (Tuple mempty (pure (NE.singleton (Box c)))) Empty -- emit box
charToCell c '*' = Just $ Tuple (Tuple mempty (pure (NE.singleton (Box c)))) Goal -- emit box
charToCell c '.' = Just $ Tuple mempty Goal
charToCell c ' ' = Just $ Tuple mempty Empty
charToCell _ _   = Nothing

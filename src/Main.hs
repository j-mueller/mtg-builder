{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Lens hiding ((...))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sequence
import Numeric.Interval

import Data
import Effects

-- | Turn a deck into a library
shuffle :: MonadRandom m => Deck -> m Library
shuffle (Deck cards) = fmap Library $ go theSeq []
  where
    theSeq =
      M.foldlWithKey'
        (\s card count -> s <> Sequence.replicate count card)
        mempty
        cards
    go theDeck lib =
      case Sequence.length theDeck of
        0 -> return lib
        n -> do
          idx <- chooseFromInterval (0 ... n)
          let card = Sequence.index theDeck idx
          let deck' = Sequence.deleteAt idx theDeck
          go deck' (card : lib)

-- | Draw a number of cards from the library
draw ::
     (Monad m, HasLibrary a, HasHand a, MonadState a m, MonadReader a m)
  => Int
  -> m ()
draw i = do
  (hd, rst) <- view $ library . unLibrary . to (splitAt i)
  library .= (Library rst)
  hand .= (Hand hd)

-- | Start the game by shuffling a deck.
startGame ::
     ( Monad m
     , HasGameState a
     , HasLibrary a
     , MonadState a m
     , MonadRandom m
     , MonadReader a m
     , HasHand a
     )
  => Deck
  -> m ()
startGame theDeck = do
  gameState .= initialState
  library <~ shuffle theDeck
  draw 7

main :: IO ()
main = do
  (finalState, _) <- evalGame (startGame myDeck)
  print finalState

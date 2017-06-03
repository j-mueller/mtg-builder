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

import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Util (putDocW)

import Mtg.Data
import qualified Mtg.Decks as Decks
import Mtg.Effects

-- | Turn a deck into a library
shuffle :: MonadRandom m => Deck -> m Library
shuffle (Deck cards) = fmap Library $ go theSeq []
  where
    theSeq =
      M.foldlWithKey'
        (\s cd (Sum count) -> s <> Sequence.replicate count cd)
        mempty
        cards
    go theDeck lib =
      case Sequence.length theDeck of
        0 -> return lib
        n -> do
          idx <- chooseFromInterval (0 ... n)
          let topCard = Sequence.index theDeck idx
          let deck' = Sequence.deleteAt idx theDeck
          go deck' (topCard : lib)

-- | Draw a number of cards from the library
draw ::
     (Monad m, HasLibrary a, HasHand a, MonadState a m, MonadReader a m)
  => Int
  -> m ()
draw i = do
  (hd, rst) <- view $ library . unLibrary . to (splitAt i)
  library .= (Library rst)
  hand .= (Hand hd)

hasLand :: (Monad m, HasHand a, MonadReader a m) => m Bool
hasLand = view $ hand . unHand . to (any isLand)

playLand ::
     ( Monad m
     , HasHand a
     , HasBattlefield a
     , MonadReader a m
     , MonadPlus m
     , MonadState a m
     )
  => m ()
playLand = do
  ld <- selectFromHand isLand
  battlefield <>= (Battlefield [ld])

-- | Start the game by shuffling a deck.
startGame ::
     ( Monad m
     , HasGameState a
     , HasLibrary a
     , MonadState a m
     , MonadRandom m
     , MonadReader a m
     , HasHand a
     , HasBattlefield a
     , MonadWriter DeckStatistics m
     , MonadPlus m
     )
  => Deck
  -> m ()
startGame theDeck = do
  gameState .= initialState
  library <~ shuffle theDeck
  draw 7
  theState <- view gameState
  tell $ stats & averageLands .~ (landsInHand theState)
  tell $ stats & averageCmc .~ (cmcInHand theState)
  l <- hasLand
  if l
    then playLand
    else return ()
  manaCurve <- view $ gameState . to getManaCurve
  tell $ stats & averageManaCurve .~ manaCurve

-- | Play a number of times and aggregate the results
-- | TODO: Return type should not be IO
simulate :: Int -> Deck -> IO DeckStatistics
simulate n d =
  fmap (foldMap snd) $ mapM (const $ evalGame (startGame d)) [1 .. n]

main :: IO ()
main = (simulate 1000 Decks.myDeck) >>= putDocW 80 . pretty

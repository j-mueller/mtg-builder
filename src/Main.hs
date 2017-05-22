{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Lens hiding ((...))
import Control.Monad.Free
import Control.Monad.Logic.Class
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid hiding (Alt)
import Data.Sequence (Seq)
import qualified Data.Sequence as Sequence
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Interval
import System.Random

newtype CardName = CardName { _unID :: Text }
  deriving (Eq, Ord, Show)

instance IsString CardName where
  fromString = CardName . fromString

newtype Deck = Deck { _unDeck :: Map CardName Int }
  deriving (Eq, Ord, Show)

newtype Library = Library { _unLibrary :: [CardName] }
  deriving (Eq, Ord, Show)

newtype Hand = Hand { _unHand :: [CardName] }
  deriving (Eq, Ord, Show)

newtype Battlefield = Battlefield { _unBattlefield :: [CardName] }
  deriving (Eq, Ord, Show)

data ManaType = Green | White | Colourless
  deriving (Eq, Ord, Show)

newtype ManaPool = ManaPool { _unPool :: Map ManaType Int }
  deriving (Eq, Ord, Show)

data GameState = GameState {
  _turnNumber :: Int,
  _hand :: Hand,
  _library :: Library,
  _battlefield :: Battlefield,
  _manaPool :: ManaPool
} deriving (Eq, Ord, Show)

makeLenses ''GameState

-- https://deckstats.net/deck-13141458-e70c57464ad14d9780bf4084fedc74c6.html
myDeck :: Deck
myDeck = Deck $ M.fromList [
  ("Anointed Procession", 4),
  ("Blisterpod", 4),
  ("Call the Scions", 4),
  ("Coretapper", 4),
  ("Dross Scorpion", 2), -- maybe remove
  ("Dryad Arbor", 4),
  ("Forest", 10),
  ("Grinding Station", 2),
  ("Llanowar Elves", 4),
  ("Llanowar Mentor", 2),
  ("Nest Invader", 4),
  ("Plains", 3),
  ("Scattered Groves", 4),
  ("Scion Summoner", 3),
  ("Soul Foundry", 2),
  ("Spawning Pit", 3),
  ("Titan Forge", 1)
  ]

data MagicF w s a =
  GetState (s -> a)
  | PutState s a
  | WriteLog w a
  | Alt [a] -- alternative (choose both)
  | Random (Interval Int) (Int -> a) -- supply a random number, inclusive in first argument, exclusive in second argument
  | Yield a -- yield to let the other player do stuff; TODO: Use existential type to enforce this at the end of every action? 
  deriving Functor

newtype Magic w s a = Magic { _unMagic :: Free (MagicF w s) a }
  deriving (Functor, Applicative, Monad)

instance MonadState s (Magic w s) where
  get = getState
  put = putState

instance Monoid w => MonadWriter w (Magic w s) where
  tell = writeLog
  -- TODO: Implement `listen` and `pass`

-- | Read the current state
getState :: Magic w s s
getState = Magic $ Free $ GetState Pure

-- | Write the current state
putState :: s -> Magic w s ()
putState s = Magic $ Free $ PutState s $ Pure ()

-- | Add an entry to the log
writeLog :: w -> Magic w s ()
writeLog w = Magic $ Free $ WriteLog w $ Pure ()

-- | Choose the best of a number of options
alt :: [Magic w s a] -> Magic w s a
alt = Magic . Free . Alt . fmap _unMagic

-- | Randomly pick one of a number of options
pick :: Int -> Int -> Magic w s Int
pick lw hi = Magic $ Free $ Random (lw ... hi) $ \i -> Pure i

yield :: Magic w s ()
yield = Magic $ Free $ Yield $ Pure ()

-- | Turn a deck into a library
shuffle :: Deck -> Magic w s Library
shuffle (Deck cards) = fmap Library $ go theSeq [] where
  theSeq = M.foldlWithKey' (\s card count -> s <> Sequence.replicate count card) mempty cards
  go deck lib = case Sequence.length deck of
    0 -> return lib
    n -> do
      idx <- pick 0 n
      let card = Sequence.index deck idx
      let deck' = Sequence.deleteAt idx deck
      go deck' (card:lib)

startingHand :: Library -> Hand
startingHand = Hand . take 7 . _unLibrary

runOne :: (MonadIO m, Monoid w, MonadState s m, MonadWriter w m, Eq s, Ord s) => Magic w s a -> m a
runOne (Magic f) = foldFree go f where
  go i = case i of
    PutState s x -> put s >> return x
    GetState f -> fmap (f $) get
    WriteLog w x -> tell w >> return x
    Alt _ -> undefined -- TODO: Here we should evaluate all branches as far as posible, then choose the one with the highest payoff
    Random intvl f -> do
      n <- liftIO $ getStdRandom (randomR (inf intvl, pred $ sup intvl))
      return $ f n
    Yield x -> return x

main :: IO ()
main = do
  let hnd = fmap startingHand $ shuffle myDeck
  (theCards, _) <- runWriterT (evalStateT (runOne hnd) ()) :: IO (Hand, ())
  putStrLn $ show theCards
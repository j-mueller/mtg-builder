{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data where

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

newtype Turn = Turn
  { _unTurn :: Int
  } deriving (Eq, Ord, Show)

makeClassy ''Turn

newtype CardName = CardName
  { _unID :: Text
  } deriving (Eq, Ord, Show)

instance IsString CardName where
  fromString = CardName . fromString

newtype Deck = Deck
  { _unDeck :: Map CardName Int
  } deriving (Eq, Ord, Show)

makeClassy ''Deck

newtype Library = Library
  { _unLibrary :: [CardName]
  } deriving (Eq, Ord, Show)

makeClassy ''Library

newtype Hand = Hand
  { _unHand :: [CardName]
  } deriving (Eq, Ord, Show)

makeClassy ''Hand

newtype Battlefield = Battlefield
  { _unBattlefield :: [CardName]
  } deriving (Eq, Ord, Show)

makeClassy ''Battlefield

data ManaType
  = Green
  | White
  | Colourless
  deriving (Eq, Ord, Show)

makePrisms ''ManaType

newtype ManaPool = ManaPool
  { _unPool :: Map ManaType Int
  } deriving (Eq, Ord, Show)

makeClassy ''ManaPool

data GameState = GameState
  { _gameStateTurn :: Turn
  , _gameStateHand :: Hand
  , _gameStateLibrary :: Library
  , _gameStateBattlefield :: Battlefield
  , _gameStateManaPool :: ManaPool
  } deriving (Eq, Ord, Show)

makeLenses ''GameState

class HasGameState a where
  gameState :: Lens' a GameState

instance HasGameState GameState where
  gameState = id

initialState :: GameState
initialState =
  GameState (Turn 0) (Hand []) (Library []) (Battlefield []) (ManaPool M.empty)

instance HasTurn GameState where
  turn = lens (view gameStateTurn) (flip $ set gameStateTurn)

instance HasHand GameState where
  hand = lens (view gameStateHand) (flip $ set gameStateHand)

instance HasLibrary GameState where
  library = lens (view gameStateLibrary) (flip $ set gameStateLibrary)

instance HasBattlefield GameState where
  battlefield =
    lens (view gameStateBattlefield) (flip $ set gameStateBattlefield)

instance HasManaPool GameState where
  manaPool = lens (view gameStateManaPool) (flip $ set gameStateManaPool)

-- https://deckstats.net/deck-13141458-e70c57464ad14d9780bf4084fedc74c6.html
myDeck :: Deck
myDeck =
  Deck $
  M.fromList
    [ ("Anointed Procession", 4)
    , ("Blisterpod", 4)
    , ("Call the Scions", 4)
    , ("Coretapper", 4)
    , ("Dross Scorpion", 2) -- maybe remove
    , ("Dryad Arbor", 4)
    , ("Forest", 10)
    , ("Grinding Station", 2)
    , ("Llanowar Elves", 4)
    , ("Llanowar Mentor", 2)
    , ("Nest Invader", 4)
    , ("Plains", 3)
    , ("Scattered Groves", 4)
    , ("Scion Summoner", 3)
    , ("Soul Foundry", 2)
    , ("Spawning Pit", 3)
    , ("Titan Forge", 1)
    ]


type GameLog = () --TODO: Proper log type
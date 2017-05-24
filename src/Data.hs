{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data where

import Control.Lens hiding ((...))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

newtype Turn = Turn
  { _unTurn :: Int
  } deriving (Eq, Ord, Show)

makeClassy ''Turn

instance Pretty Turn where
  pretty = pretty . view unTurn

newtype CardName = CardName
  { _unID :: Text
  } deriving (Eq, Ord, Show)

makeLenses ''CardName

instance Pretty CardName where
  pretty = pretty . view unID

instance IsString CardName where
  fromString = CardName . fromString

newtype Deck = Deck
  { _unDeck :: Map CardName Int
  } deriving (Eq, Ord, Show)

makeClassy ''Deck

instance Pretty Deck where
  pretty =
    foldMap (\(card, count) -> pretty count <> space <> pretty card <> line) .
    M.toList .
    view unDeck

newtype Library = Library
  { _unLibrary :: [CardName]
  } deriving (Eq, Ord, Show)

makeClassy ''Library

instance Pretty Library where
  pretty =
    encloseSep emptyDoc " (...)" (comma <> space) . fmap pretty . take 5 . view unLibrary

newtype Hand = Hand
  { _unHand :: [CardName]
  } deriving (Eq, Ord, Show)

makeClassy ''Hand

instance Pretty Hand where
  pretty = vsep . punctuate comma . fmap pretty . view unHand

newtype Battlefield = Battlefield
  { _unBattlefield :: [CardName]
  } deriving (Eq, Ord, Show)

makeClassy ''Battlefield

instance Pretty Battlefield where
  pretty = vsep . punctuate comma . fmap pretty . view unBattlefield

data ManaType
  = Green
  | White
  | Colourless
  deriving (Eq, Ord, Show)

makePrisms ''ManaType

instance Pretty ManaType where
  pretty Green = "{G}"
  pretty White = "{W}"
  pretty Colourless = "{C}"

newtype ManaPool = ManaPool
  { _unPool :: Map ManaType Int
  } deriving (Eq, Ord, Show)

makeClassy ''ManaPool

instance Pretty ManaPool where
  pretty =
    vsep . punctuate comma .
    fmap (\(tpe, count) -> pretty count <> space <> pretty tpe <> line) .
    M.toList .
    view unPool

data GameState = GameState
  { _gameStateTurn :: Turn
  , _gameStateHand :: Hand
  , _gameStateLibrary :: Library
  , _gameStateBattlefield :: Battlefield
  , _gameStateManaPool :: ManaPool
  } deriving (Eq, Ord, Show)

makeLenses ''GameState

instance Pretty GameState where
  pretty gs =
    vsep
      [ "Turn:" <> space <> pretty (gs ^. turn)
      , nest
          2
          (vsep
             [ "Hand (" <> (pretty $ length $ gs ^. hand . unHand) <> "):"
             , pretty (gs ^. hand)
             ])
      , nest
          2
          (vsep
             [ "Library (" <> (pretty $ length $ gs ^. library . unLibrary) <>
               "):"
             , pretty (gs ^. library)
             ])
      , nest
          2
          (vsep
             [ "Battlefield (" <>
               (pretty $ length $ gs ^. battlefield . unBattlefield) <>
               "):"
             , pretty (gs ^. battlefield)
             ])
      , nest 4 (vsep ["Mana pool (:", pretty (gs ^. manaPool)])
      ]

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

isLand :: CardName -> Bool
isLand (CardName n) = case n of
  "Forest" -> True
  "Plains" -> True
  _ -> False

newtype AvgLands = AvgLands { _unAvgLands :: (Sum Integer, Sum Integer) }
  deriving (Eq, Ord, Show, Monoid)

instance Semigroup AvgLands where
  (<>) = mappend

getAvgLands ::  AvgLands -> Double
getAvgLands (AvgLands (Sum count, Sum total)) = 
  (fromInteger count) / (fromInteger total)

instance Pretty AvgLands where
  pretty = pretty . getAvgLands

landsInHand :: GameState -> AvgLands
landsInHand gs = AvgLands (Sum lands, 1) where
  lands = gs^.hand.unHand.to (toInteger . length . filter isLand)

data DeckStatistics = DeckStatistics {
  _averageLands :: AvgLands
} deriving (Eq, Ord, Show)

makeLenses ''DeckStatistics

instance Monoid DeckStatistics where
  mempty = DeckStatistics mempty
  l `mappend` r = DeckStatistics $ (l^.averageLands) <> (r^.averageLands)

instance Pretty DeckStatistics where
  pretty ds = vsep [
    "Lands in hand (avg): " <> (pretty $ ds^.averageLands)
    ]

computeStats :: GameState -> DeckStatistics
computeStats = DeckStatistics <$> landsInHand
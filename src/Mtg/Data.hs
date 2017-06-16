{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mtg.Data where

import           Control.Lens              hiding ((...))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Semigroup
import           Data.String               (IsString (..))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

newtype Turn = Turn
  { _unTurn :: Int
  } deriving (Eq, Ord, Show)

makeClassy ''Turn

instance Pretty Turn where
  pretty = pretty . view unTurn

newtype CardName = CardName
  { _unID :: Text
  } deriving (Eq, Ord, Show)

makeClassy ''CardName

instance Pretty CardName where
  pretty = pretty . view unID

instance IsString CardName where
  fromString = CardName . fromString

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

data ManaSymbol
  = ColourSymbol ManaType
  | XSymbol
  deriving (Eq, Ord, Show)

instance Pretty ManaSymbol where
  pretty s =
    case s of
      ColourSymbol t -> pretty t
      XSymbol -> "{X}"

makePrisms ''ManaSymbol

newtype ManaCost = ManaCost
  { _unManaCost :: Map ManaSymbol (Sum Int)
  } deriving (Eq, Ord, Show, Monoid)

instance Semigroup ManaCost where
  (<>) = mappend

makeClassy ''ManaCost

instance Pretty ManaCost where
  pretty =
    foldMap (\(smb, Sum count) -> foldMap pretty $ replicate count smb) .
    M.toList .
    view unManaCost

nOf :: Int -> ManaType -> ManaCost
nOf i t = ManaCost $ M.fromList [(ColourSymbol t, Sum i)]

oneOf :: ManaType -> ManaCost
oneOf = nOf 1

twoOf :: ManaType -> ManaCost
twoOf = nOf 2

threeOf :: ManaType -> ManaCost
threeOf = nOf 3

-- | Compute the converted mana cost of a card
cmc :: ManaCost -> Int
cmc =
  getSum .
  foldMap
    (\(smb, cnt) ->
       case smb of
         ColourSymbol _ -> cnt
         _ -> 0) .
  M.toList .
  _unManaCost

data Card = Card
  { _cCardName :: CardName
  , _cManaCost :: ManaCost
  } deriving (Eq, Ord, Show)

makeLenses ''Card

instance HasCardName Card where
  cardName = cCardName

instance HasManaCost Card where
  manaCost = cManaCost

instance Pretty Card where
  pretty = pretty . view cardName

card :: CardName -> ManaCost -> Card
card = Card

newtype Deck = Deck
  { _unDeck :: Map Card (Sum Int)
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''Deck

instance Pretty Deck where
  pretty =
    foldMap (\(cd, Sum count) -> pretty count <> space <> pretty cd <> line) .
    M.toList .
    view unDeck

newtype Library = Library
  { _unLibrary :: [Card]
  } deriving (Eq, Ord, Show)

makeClassy ''Library

instance Pretty Library where
  pretty =
    encloseSep emptyDoc " (...)" (comma <> space) . fmap pretty . take 5 .
    view unLibrary

newtype Hand = Hand
  { _unHand :: [Card]
  } deriving (Eq, Ord, Show)

makeClassy ''Hand

instance Pretty Hand where
  pretty = vsep . punctuate comma . fmap pretty . view unHand

newtype Battlefield = Battlefield
  { _unBattlefield :: [Card]
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''Battlefield

instance Pretty Battlefield where
  pretty = vsep . punctuate comma . fmap pretty . view unBattlefield

newtype ManaPool = ManaPool
  { _unPool :: Map ManaType (Sum Int)
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''ManaPool

instance Pretty ManaPool where
  pretty =
    vsep . punctuate comma .
    fmap (\(tpe, Sum count) -> pretty count <> space <> pretty tpe <> line) .
    M.toList .
    view unPool

data GameState = GameState
  { _gameStateTurn        :: Turn
  , _gameStateHand        :: Hand
  , _gameStateLibrary     :: Library
  , _gameStateBattlefield :: Battlefield
  , _gameStateManaPool    :: ManaPool
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
  GameState (Turn 1) (Hand []) (Library []) (Battlefield []) (ManaPool M.empty)

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

isLand :: Card -> Bool -- TODO: Card types (basic land, creature, etc.)
isLand c =
  case c ^. cardName of
    "Forest" -> True
    "Plains" -> True
    _ -> False


-- | Assign a numeric score to the state of the game
-- TODO: Should be something better than a constant value
score :: GameState -> Sum Int
score = const 10

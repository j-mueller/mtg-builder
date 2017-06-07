-- | Some decks that can be analysed
module Mtg.Decks where

import qualified Data.Map.Strict as M
import qualified Mtg.Cards       as Cards
import           Mtg.Data

-- https://deckstats.net/deck-13141458-e70c57464ad14d9780bf4084fedc74c6.html
myDeck :: Deck
myDeck =
  Deck $
  M.fromList
    [ (Cards.anointedProcession, 4)
    , (Cards.llanowarElves, 4)
    , (Cards.forest, 10)
    , (Cards.plains, 4)
    ]
    -- , ("Blisterpod", 4)
    -- , ("Call the Scions", 4)
    -- , ("Coretapper", 4)
    -- , ("Dross Scorpion", 2) -- maybe remove
    -- , ("Dryad Arbor", 4)
    -- , ("Forest", 10)
    -- , ("Grinding Station", 2)
    -- , ("Llanowar Elves", 4)
    -- , ("Llanowar Mentor", 2)
    -- , ("Nest Invader", 4)
    -- , ("Plains", 3)
    -- , ("Scattered Groves", 4)
    -- , ("Scion Summoner", 3)
    -- , ("Soul Foundry", 2)
    -- , ("Spawning Pit", 3)
    -- , ("Titan Forge", 1)
    -- ]

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
    [ (Cards.dryadArbor, 4)
    , (Cards.llanowarElves, 2)
    , (Cards.forest, 14)
    , (Cards.lotusCobra, 4)
    , (Cards.doublingSeason, 2)
    , (Cards.rootOut, 2)
    , (Cards.panharmonicon, 2)
    , (Cards.scuteMob, 1)
    , (Cards.soulFoundry, 4)
    , (Cards.tirelessTracker, 4)
    , (Cards.vinelasherKudzu, 2)
    , (Cards.scytheLeopard, 3)
    , (Cards.rampagingBaloths, 2)
    , (Cards.swellOfGrowth, 1)
    , (Cards.hazeOfPollen, 1)
    , (Cards.evolvingWilds, 4)
    , (Cards.confrontTheUnknown, 2)
    , (Cards.oranRiefHydra, 2)
    , (Cards.loamDryad, 2)
    , (Cards.vinesOfTheRecluse, 2)
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

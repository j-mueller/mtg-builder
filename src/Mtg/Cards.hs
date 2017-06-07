{-# LANGUAGE OverloadedStrings #-}

-- | A collection of MTG cards tp be used in decks
module Mtg.Cards where

import           Data.Semigroup
import           Mtg.Data

anointedProcession :: Card
anointedProcession =
  card "Anointed Procession" (threeOf Colourless <> oneOf White)

llanowarElves :: Card
llanowarElves = card "Llanowar Elves" (oneOf Green)

forest :: Card
forest = card "Forest" mempty

plains :: Card
plains = card "Plains" mempty

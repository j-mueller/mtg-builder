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

dryadArbor :: Card
dryadArbor = card "Dryad Arbor" mempty

lotusCobra :: Card
lotusCobra = card "Lotus Cobra" (oneOf Colourless <> oneOf Green)

doublingSeason :: Card
doublingSeason = card "Doubling Season" (oneOf Green <> fourOf Colourless)

rootOut :: Card
rootOut = card "Root Out" (oneOf Colourless <> oneOf Green) -- TODO: Check mana cost

panharmonicon :: Card
panharmonicon = card "Panharmonicon" (fourOf Colourless)

scuteMob :: Card
scuteMob = card "Scute Mob" (oneOf Green)

soulFoundry :: Card
soulFoundry = card "Soul Foundry" (fourOf Colourless)

tirelessTracker :: Card
tirelessTracker = card "Tireless Tracker" (oneOf Green <> fourOf Colourless)

vinelasherKudzu :: Card
vinelasherKudzu = card "Vinelasher Kudzu" (oneOf Green <> oneOf Colourless)

scytheLeopard :: Card
scytheLeopard = card "Scythe Leopard" (oneOf Green)

rampagingBaloths :: Card
rampagingBaloths = card "Rampaging Baloths" (fourOf Colourless <> twoOf Green)

swellOfGrowth :: Card
swellOfGrowth = card "Swell of Growth" (oneOf Green <> oneOf Colourless) -- TODO: Check

hazeOfPollen :: Card
hazeOfPollen = card "Haze of Pollen" (oneOf Green <> oneOf Colourless)

evolvingWilds :: Card
evolvingWilds = card "Evolving Wilds" mempty

confrontTheUnknown :: Card
confrontTheUnknown = card "Confront the Unknown" (oneOf Green)

oranRiefHydra :: Card
oranRiefHydra = card "Oran-Rief Hydra" (fourOf Colourless <> twoOf Green)

loamDryad :: Card
loamDryad = card "Loam Dryad" (oneOf Green)

vinesOfTheRecluse :: Card
vinesOfTheRecluse = card "Vines of the Recluse" (oneOf Colourless <> oneOf Green) -- TODO: Check
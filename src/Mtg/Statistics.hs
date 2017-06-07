{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mtg.Statistics where

import           Control.Lens              hiding ((...))
import           Control.Monad.Reader
import           Control.Monad.Writer      hiding (Alt, (<>))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Semigroup
import           Data.Text.Prettyprint.Doc

import           Mtg.Data

newtype Distribution k = Distribution
  { _unDistribution :: Map k (Sum Integer)
  } deriving (Eq, Ord, Show)

instance Ord k => Semigroup (Distribution k) where
  (<>) = mappend

instance Ord k => Monoid (Distribution k) where
  mempty = Distribution M.empty
  (Distribution l) `mappend` (Distribution r) =
    Distribution $ M.mergeWithKey (\_ ll rr -> Just $ ll <> rr) id id l r

-- TODO: Discrete distribution (Map a (Sum Integer))
newtype AvgLands = AvgLands
  { _unAvgLands :: (Sum Integer, Sum Integer)
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''AvgLands

instance Semigroup AvgLands where
  (<>) = mappend

getAvgLands :: AvgLands -> Double
getAvgLands (AvgLands (Sum count, Sum total)) =
  (fromInteger count) / (fromInteger total)

instance Pretty AvgLands where
  pretty = pretty . getAvgLands

logLandsInHand ::
     ( Monad m
     , MonadReader a m
     , HasHand a
     , Monoid b
     , HasAvgLands b
     , MonadWriter b m
     )
  => m ()
logLandsInHand = count >>= write
  where
    count = view $ hand . unHand . to (toInteger . length . filter isLand)
    write i = tell $ mempty & avgLands .~ (AvgLands (Sum i, 1))

-- | Avg. converted mana cost of starting hand
newtype AvgCmc = AvgCmc
  { _unAvgCmc :: (Sum Integer, Sum Integer)
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''AvgCmc

instance Semigroup AvgCmc where
  (<>) = mappend

getAvgCmc :: AvgCmc -> Double
getAvgCmc (AvgCmc (Sum count, Sum total)) =
  (fromInteger count) / (fromInteger total)

instance Pretty AvgCmc where
  pretty = pretty . getAvgCmc

logConvertedManaCost ::
     ( Monad m
     , MonadReader a m
     , HasHand a
     , Monoid b
     , HasAvgCmc b
     , MonadWriter b m
     )
  => m ()
logConvertedManaCost = count >>= write
  where
    count = do
      cmc <-
        view $
        hand .
        unHand .
        to
          (toInteger .
           getSum . foldMap (Sum . convertedManaCost . view manaCost))
      cnt <- view $ hand . unHand . to length
      return (Sum cmc, Sum $ toInteger cnt)
    write t = tell $ mempty & avgCmc .~ (AvgCmc t)

-- | How much mana can we produce in each turn?
newtype ManaCurve = ManaCurve
  { _unManaCurve :: Map Turn (Sum Integer)
  } deriving (Eq, Ord, Show)

makeClassy ''ManaCurve

instance Semigroup ManaCurve where
  (<>) = mappend

instance Monoid ManaCurve where
  mempty = ManaCurve M.empty
  (ManaCurve l) `mappend` (ManaCurve r) =
    ManaCurve $ M.mergeWithKey (\_ ll rr -> Just $ ll <> rr) id id l r

newtype AvgManaCurve = AvgManaCurve
  { _unAvgManaCurve :: (Sum Integer, ManaCurve)
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''AvgManaCurve

instance Semigroup AvgManaCurve where
  (<>) = mappend

getManaCurve ::
     (HasTurn a, HasBattlefield a, Monad m, MonadReader a m) => m AvgManaCurve
getManaCurve = do
  t <- view turn
  mana <-
    view $ battlefield . unBattlefield . to (toInteger . length . filter isLand)
  return $ AvgManaCurve (Sum 1, ManaCurve $ M.fromList [(t, Sum mana)])

getAvgManaCurve :: AvgManaCurve -> Map Turn Double
getAvgManaCurve (AvgManaCurve (Sum n, ManaCurve mp)) =
  fmap (flip (/) (fromInteger n) . fromInteger . getSum) mp

instance Pretty AvgManaCurve where
  pretty a = vsep $ fmap (proc . Turn) [1 .. 5]
    where
      averages = getAvgManaCurve a
      proc t =
        "Turn " <> pretty t <> " : " <> (maybe "" pretty $ M.lookup t averages)

logAvgManaCurve ::
     ( Monad m
     , MonadReader a m
     , HasTurn a
     , HasBattlefield a
     , Monoid b
     , HasAvgManaCurve b
     , MonadWriter b m
     )
  => m ()
logAvgManaCurve = getManaCurve >>= write
  where
    write m = tell $ mempty & avgManaCurve .~ m

data DeckStatistics = DeckStatistics
  { _averageLands     :: AvgLands
  , _averageCmc       :: AvgCmc
  , _averageManaCurve :: AvgManaCurve
  } deriving (Eq, Ord, Show)

makeLenses ''DeckStatistics

instance HasAvgLands DeckStatistics where
  avgLands = averageLands

instance HasAvgCmc DeckStatistics where
  avgCmc = averageCmc

instance HasAvgManaCurve DeckStatistics where
  avgManaCurve = averageManaCurve

stats :: DeckStatistics
stats = mempty

instance Monoid DeckStatistics where
  mempty = DeckStatistics mempty mempty mempty
  l `mappend` r = DeckStatistics ll cc mp
    where
      ll = (l ^. averageLands) <> (r ^. averageLands)
      cc = (l ^. averageCmc) <> (r ^. averageCmc)
      mp = (l ^. averageManaCurve) <> (r ^. averageManaCurve)

instance Pretty DeckStatistics where
  pretty ds =
    vsep
      [ "Lands in hand (avg): " <> (pretty $ ds ^. averageLands)
      , "Converted mana cost (avg): " <> (pretty $ ds ^. averageCmc)
      , "Mana curve (avg):" <> (pretty $ ds ^. averageManaCurve)
      ]

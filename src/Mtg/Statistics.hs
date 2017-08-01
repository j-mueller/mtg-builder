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
import           Data.Ratio
import           Data.Semigroup
import           Data.Text.Prettyprint.Doc

import           Mtg.Data

-- | A discrete distribution
newtype Distribution k v = Distribution
  { _unDistribution :: Map k v
  } deriving (Eq, Ord, Show, Functor)

instance (Semigroup v, Ord k) => Semigroup (Distribution k v) where
  (<>) (Distribution l) (Distribution r) =
    Distribution $ M.mergeWithKey (\_ ll rr -> Just $ ll <> rr) id id l r

instance (Monoid v, Semigroup v, Ord k) => Monoid (Distribution k v) where
  mempty = Distribution M.empty
  mappend = (<>)

toDist :: (a -> (k, v)) -> a -> Distribution k v
toDist f = Distribution . uncurry M.singleton . f

getAverage :: Integral n => Distribution n n -> Ratio n
getAverage (Distribution ds) = sum % count
  where
    sum = getSum $ M.foldMapWithKey (\k v -> Sum $ k * v) ds
    count = getSum $ foldMap Sum ds

prettyTurnDistribution :: Pretty a => Distribution Turn a -> Doc ann
prettyTurnDistribution (Distribution a) = vsep $ fmap (proc . Turn) [1 .. 5]
  where
    proc t = "Turn " <> pretty t <> ": " <> (maybe "" pretty $ M.lookup t a)

newtype TurnDistribution = TurnDistribution
  { _unTurnDistribution :: Distribution Turn (Distribution Integer (Sum Integer))
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''TurnDistribution

instance Semigroup TurnDistribution where
  (<>) = mappend

countTurn :: Turn -> Integer -> TurnDistribution
countTurn t i = TurnDistribution $ Distribution $ M.fromList [(t, Distribution $ M.fromList [(i, Sum 1)])]

instance Pretty TurnDistribution where
  pretty = prettyTurnDistribution . fmap (fromRational :: Rational -> Double) . fmap getAverage . fmap (fmap getSum) . view unTurnDistribution

newtype LandsInHand = LandsInHand
  { _unLandsInHand :: TurnDistribution
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''LandsInHand

countLands :: Turn -> Integer -> LandsInHand
countLands t = LandsInHand . countTurn t

instance Semigroup LandsInHand where
  (<>) = mappend

instance Pretty LandsInHand where
  pretty = pretty . view unLandsInHand

logLandsInHand ::
     ( Monad m
     , MonadReader a m
     , HasHand a
     , HasTurn a
     , Monoid b
     , HasLandsInHand b
     , MonadWriter b m
     )
  => m ()
logLandsInHand = count >>= write
  where
    count = do
      t <- view turn
      lds <- view $ hand . unHand . to (toInteger . length . filter isLand)
      return $ countLands t lds
    write i = tell $ mempty & landsInHand .~ i

newtype CardsInHand = CardsInHand
  { _unCardsInHand :: TurnDistribution
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''CardsInHand

countCards :: Turn -> Integer -> CardsInHand
countCards t = CardsInHand . countTurn t

instance Semigroup CardsInHand where
  (<>) = mappend

instance Pretty CardsInHand where
  pretty = pretty . view unCardsInHand

logCardsInHand ::
     ( Monad m
     , MonadReader a m
     , HasHand a
     , HasTurn a
     , Monoid b
     , HasCardsInHand b
     , MonadWriter b m
     )
  => m ()
logCardsInHand = count >>= write
  where
    count = do
      t <- view turn
      lds <- view $ hand . unHand . to (toInteger . length)
      return $ countCards t lds
    write i = tell $ mempty & cardsInHand .~ i

-- | Avg. converted mana cost of starting hand
newtype ConvertedManaCost = ConvertedManaCost
  { _unConvertedManaCost :: TurnDistribution
  } deriving (Eq, Ord, Show)

makeClassy ''ConvertedManaCost

instance Semigroup ConvertedManaCost where
  (<>) = mappend

instance Monoid ConvertedManaCost where
  mempty = ConvertedManaCost mempty
  (ConvertedManaCost l) `mappend` (ConvertedManaCost r) =
    ConvertedManaCost $ l <> r

instance Pretty ConvertedManaCost where
  pretty = pretty . view unConvertedManaCost

logConvertedManaCost ::
     ( Monad m
     , MonadReader a m
     , HasHand a
     , HasTurn a
     , Monoid b
     , HasConvertedManaCost b
     , MonadWriter b m
     )
  => m ()
logConvertedManaCost = count >>= write
  where
    count = do
      t <- view turn
      cards <- view $ hand . unHand
      let cc =
            foldMap
              (countTurn t . toInteger . cmc . view cManaCost)
              cards
      return $ ConvertedManaCost cc
    write t = tell $ mempty & convertedManaCost .~ t

-- | How much mana can we produce in each turn?
newtype ManaCurve = ManaCurve
  { _unManaCurve :: TurnDistribution
  } deriving (Eq, Ord, Show, Monoid)

makeClassy ''ManaCurve

instance Semigroup ManaCurve where
  (<>) = mappend

getManaCurve ::
     (HasTurn a, HasBattlefield a, Monad m, MonadReader a m) => m ManaCurve
getManaCurve = do
  t <- view turn
  mana <-
    view $ battlefield . unBattlefield . to (toInteger . length . filter isLand)
  return $  ManaCurve $ countTurn t mana

instance Pretty ManaCurve where
  pretty = pretty . view unManaCurve

logAvgManaCurve ::
     ( Monad m
     , MonadReader a m
     , HasTurn a
     , HasBattlefield a
     , Monoid b
     , HasManaCurve b
     , MonadWriter b m
     )
  => m ()
logAvgManaCurve = getManaCurve >>= write
  where
    write m = tell $ mempty & manaCurve .~ m

data DeckStatistics = DeckStatistics
  { _dsLandsInHand       :: !LandsInHand
  , _dsConvertedManaCost :: !ConvertedManaCost
  , _dsManaCurve         :: !ManaCurve
  , _dsCardsInHand       :: !CardsInHand
  } deriving (Eq, Ord, Show)

makeLenses ''DeckStatistics

instance HasLandsInHand DeckStatistics where
  landsInHand = dsLandsInHand

instance HasConvertedManaCost DeckStatistics where
  convertedManaCost = dsConvertedManaCost

instance HasManaCurve DeckStatistics where
  manaCurve = dsManaCurve

instance HasCardsInHand DeckStatistics where
  cardsInHand = dsCardsInHand

stats :: DeckStatistics
stats = mempty

instance Monoid DeckStatistics where
  mempty = DeckStatistics mempty mempty mempty mempty
  l `mappend` r = DeckStatistics ll cc mp ch
    where
      ll = (l ^. landsInHand) <> (r ^. landsInHand)
      cc = (l ^. convertedManaCost) <> (r ^. convertedManaCost)
      mp = (l ^. manaCurve) <> (r ^. manaCurve)
      ch = (l ^. cardsInHand) <> (r ^. cardsInHand)


instance Pretty DeckStatistics where
  pretty ds =
    vsep
      [ nest 2 $  "Lands in hand (avg): " <> line <> (pretty $ ds ^. landsInHand)
      , nest 2 $ "Converted mana cost (avg): " <> line <> (pretty $ ds ^. convertedManaCost)
      , nest 2 $ "Mana curve (avg):" <> line <> (pretty $ ds ^. manaCurve)
      , nest 2 $ "Cards in hand (avg): " <> line <> (pretty $ ds ^. cardsInHand)
      ]

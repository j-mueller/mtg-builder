{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Mtg.Effects where

import           Control.Applicative
import           Control.Lens         hiding ((...))
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer hiding (Alt)
import           Data.List            (partition)
import           Numeric.Interval     hiding (empty)
import           System.Random

import           Mtg.Data

data Magic w s a
  = GetState (s -> Magic w s a)
  | PutState s
             (Magic w s a)
  | WriteLog w
             (Magic w s a)
  | Alt [Magic w s a]
  | Random (Interval Int)
           (Int -> Magic w s a) -- supply a random number, inclusive in first argument, exclusive in second argument
  | Yield a
  deriving (Functor)

instance Monoid w => Applicative (Magic w s) where
  pure = Yield
  (<*>) = ap

instance Monoid w => Monad (Magic w s) where
  l >>= r =
    case l of
      GetState f -> GetState $ \s -> f s >>= r
      PutState s m -> PutState s $ m >>= r
      WriteLog w m -> WriteLog w $ m >>= r
      Alt ms -> Alt $ fmap (flip (>>=) r) ms
      Random intvl f -> Random intvl $ \i -> f i >>= r
      Yield a -> r a

instance Monoid w => MonadState s (Magic w s) where
  get = getState
  put = putState

instance Monoid w => MonadWriter w (Magic w s) where
  tell = writeLog
  -- TODO: Implement `listen` and `pass`

class Monad m =>
      MonadRandom m where
  chooseFromInterval :: Interval Int -> m Int

instance Monoid w => MonadRandom (Magic w s) where
  chooseFromInterval i = Random i Yield

focus :: [a] -> [(a, [a])]
focus [] = []
focus (x:xs) = (x, xs) : go [x] xs
  where
    go _ [] = []
    go lefts (y:ys) = (y, lefts) : go (y : lefts) ys

instance Monoid w => Alternative (Magic w s) where
  empty = Alt []
  l <|> r = Alt [l, r]

instance Monoid w => MonadPlus (Magic w s) where
  mzero = Alt []
  mplus l r = Alt [l, r]

-- | Pick all elements of a non-empty list using the monad choice effect
pickAll :: MonadPlus m => [a] -> m (a, [a])
pickAll = msum . fmap return . focus

instance Monoid w => MonadReader s (Magic w s) where
  ask = getState
  local f m = do
    old <- getState
    putState $ f old
    m

-- | Read the current state
getState :: Magic w s s
getState = GetState Yield

-- | Write the current state
putState :: s -> Magic w s ()
putState s = PutState s $ Yield ()

-- | Add an entry to the log
writeLog :: w -> Magic w s ()
writeLog w = WriteLog w $ Yield ()

-- | Choose the best of a number of options
alt :: [Magic w s a] -> Magic w s a
alt = Alt

-- | Randomly pick one of a number of options
pick :: Int -> Int -> Magic w s Int
pick lw hi = Random (lw ... hi) Yield

yield :: Magic w s ()
yield = Yield ()

runMagic ::
     ( MonadIO m
     , Monoid w
     , MonadState s m
     , MonadWriter w m
     , Eq t
     , Ord t
     , Monoid t
     )
  => (s -> t)
  -> Magic w s ()
  -> m ()
runMagic cmp ff = go ff
  where
    go g =
      case g of
        PutState s x -> put s >> go x
        GetState f -> do
          theState <- get
          go (f theState)
        WriteLog w x -> tell w >> go x
        Alt branches -> get >>= go . pickBranch cmp . runMagics branches
        Random intvl f -> do
          n <- liftIO $ getStdRandom (randomR (inf intvl, pred $ sup intvl))
          go (f n)
        Yield x -> return x

-- | Run some alternatives as far as possible (until IO is hit)
runMagics :: Monoid w => [Magic w s a] -> s -> [(s, Magic w s a)]
runMagics branches currentState = branches >>= runBranch currentState

runBranch :: Monoid w => s -> Magic w s a -> [(s, Magic w s a)]
runBranch currentState f =
  case f of
    PutState s x -> runBranch s x
    GetState ff -> runBranch currentState (ff currentState)
    WriteLog w x -> runBranch currentState (tell w >> x)
    Alt branches -> branches >>= runBranch currentState
    Random _ _ -> [(currentState, f)]
    Yield _ -> [(currentState, f)]

-- | Perform a linear scan of a list of branches and return the best one
-- according to some discriminator `t`. The empty element of the `Monoid t`
-- instance should be the bottom element under `Ord t`.
pickBranch ::
     (Monoid w, Eq t, Ord t, Monoid t)
  => (s -> t)
  -> [(s, Magic w s a)]
  -> Magic w s a
pickBranch _ [] = Alt []
pickBranch ev (x:xs) = go x (ev $ fst x) xs
  where
    go (st, mg) _ [] = putState st >> mg
    go (st, mg) t ((st', mg'):ys) =
      let t' = ev st'
      in if (t' > t)
           then go (st', mg') t' ys
           else go (st, mg) t ys

evalGame :: Monoid w => Magic w GameState () -> IO (GameState, w)
evalGame mm = runWriterT (execStateT (runMagic score mm) initialState)

selectFromHand ::
     ( Monad m
     , HasHand a
     , HasHand a
     , MonadReader a m
     , MonadPlus m
     , MonadState a m
     )
  => (Card -> Bool)
  -> m Card
selectFromHand pred = do
  (good, bad) <- view $ hand . unHand . to (partition pred)
  case good of
    [] -> empty
    x:xs -> do
      hand .= (Hand $ bad ++ xs)
      return x

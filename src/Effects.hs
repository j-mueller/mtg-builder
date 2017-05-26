{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Effects where

import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import Numeric.Interval
import System.Random

import Data

data MagicF w s a
  = GetState (s -> a)
  | PutState s
             a
  | WriteLog w
             a
  | Alt [a] -- alternative (choose both)
  | Random (Interval Int)
           (Int -> a) -- supply a random number, inclusive in first argument, exclusive in second argument
  | Yield a -- yield to let the other player do stuff; TODO: Use existential type to enforce this at the end of every action? 
  deriving (Functor)

newtype Magic w s a = Magic
  { _unMagic :: Free (MagicF w s) a
  } deriving (Functor, Applicative, Monad)

instance MonadState s (Magic w s) where
  get = getState
  put = putState

instance Monoid w => MonadWriter w (Magic w s) where
  tell = writeLog
  -- TODO: Implement `listen` and `pass`

class Monad m =>
      MonadRandom m where
  chooseFromInterval :: Interval Int -> m Int

instance MonadRandom (Magic w s) where
  chooseFromInterval i = Magic $ Free $ Random i Pure

instance MonadReader s (Magic w s) where
  ask = getState
  local f m = do
    old <- getState
    putState $ f old
    m

-- | Read the current state
getState :: Magic w s s
getState = Magic $ Free $ GetState Pure

-- | Write the current state
putState :: s -> Magic w s ()
putState s = Magic $ Free $ PutState s $ Pure ()

-- | Add an entry to the log
writeLog :: w -> Magic w s ()
writeLog w = Magic $ Free $ WriteLog w $ Pure ()

-- | Choose the best of a number of options
alt :: [Magic w s a] -> Magic w s a
alt = Magic . Free . Alt . fmap _unMagic

-- | Randomly pick one of a number of options
pick :: Int -> Int -> Magic w s Int
pick lw hi = Magic $ Free $ Random (lw ... hi) $ \i -> Pure i

yield :: Magic w s ()
yield = Magic $ Free $ Yield $ Pure ()

runMagic ::
     (MonadIO m, Monoid w, MonadState s m, MonadWriter w m, Eq s, Ord s)
  => Magic w s a
  -> m a
runMagic (Magic f) = foldFree go f
  where
    go i =
      case i of
        PutState s x -> put s >> return x
        GetState ff -> fmap (ff $) get
        WriteLog w x -> tell w >> return x
        Alt _ -> undefined -- TODO: Here we should evaluate all branches as far as posible, then choose the one with the highest payoff
        Random intvl f -> do
          n <- liftIO $ getStdRandom (randomR (inf intvl, pred $ sup intvl))
          return $ f n
        Yield x -> return x

evalGame :: Magic GameLog GameState () -> IO (GameState, GameLog)
evalGame mm = runWriterT (execStateT (runMagic mm) initialState)

module Utils(
    sleep
  , waitForever
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.IO.Class

import Data.Time

-- | Stop the thread for some time.
sleep :: MonadIO m => NominalDiffTime -> m ()
sleep dt = liftIO . delay $ toMicroseconds dt

-- | Convert time to microseconds
toMicroseconds :: NominalDiffTime -> Integer
toMicroseconds t = ceiling $ toRational t * 1000000

-- | Stop the thread forever
waitForever :: IO ()
waitForever = do
  forever $ threadDelay maxBound
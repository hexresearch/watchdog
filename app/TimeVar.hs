module TimeVar(
    TimeVar
  , initTimeVar
  , timePassed
  , updateTime
) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Time

newtype TimeVar = TimeVar { unTimeVar :: TVar UTCTime }

initTimeVar :: IO TimeVar
initTimeVar = fmap TimeVar $ newTVarIO =<< getCurrentTime

timePassed :: TimeVar -> IO NominalDiffTime
timePassed (TimeVar tv) = do
  lastTimePing <- readTVarIO tv
  timeNow <- getCurrentTime
  return $ diffUTCTime timeNow lastTimePing

updateTime :: TimeVar -> IO ()
updateTime (TimeVar tv) = atomically . writeTVar tv =<< getCurrentTime

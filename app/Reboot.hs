module Reboot(
  reboot
) where

import Control.Monad
import Control.Exception
import Data.Time
import Text.InterpolatedString.Perl6 (qc)
import Text.Read
import qualified Data.Text as T

import System.ConfigApp
import System.Process

import Config
import Utils

reboot :: Config -> IO ()
reboot config = do
  when isDebug $ do
    mapM_ (\file -> maybe (return ()) print =<< getLastRebootTime file) mLogFile
    putStrLn "PC will be restarted in 10 seconds."
    putStrLn "Type Ctrl+C to cancel reboot."
    sleep 10
  mapM_ (\file -> (putLastRebootTime file) =<< getCurrentTime) mLogFile
  void $ system (T.unpack $ appRebootCmd config)
  where
    isDebug = maybe False id $ appDebugMode config
    mLogFile = fmap unConfigPath $ appLastRebootTimeFilePath config

putLastRebootTime :: FilePath -> UTCTime -> IO ()
putLastRebootTime filename time = do
  ewriteResult <- try $ writeFile filename $ show time
  case ewriteResult of
    Left (except :: IOError) -> putStrLn [qc|Error writing file {filename}: {except}|] >> return ()
    _ -> return ()

getLastRebootTime :: FilePath -> IO (Maybe UTCTime)
getLastRebootTime filename = do
  econtents <- try $ readFile filename
  case econtents of
    Left (except :: IOError) -> putStrLn [qc|Error reading file {filename}: {except}|] >> return Nothing
    Right contents -> pure $ readMaybe contents



module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Network.Simple.TCP
import System.Directory
import System.ConfigApp
import Text.InterpolatedString.Perl6 (qc)

import qualified Data.Text as T

import Config
import Utils
import Reboot
import TimeVar

main :: IO ()
main = configApp desc runConfig
  where
    desc = AppDesc
      { appName = "watchdog"
      , appDesc = "Restarts PC if no signal is sent over TCP" }

runConfig :: Config -> IO ()
runConfig cfg = checkConfig cfg $ do
  tv <- initTimeVar
  runTcpListener  cfg tv
  runRebootWorker cfg tv
  waitForever

checkConfig :: Config -> IO () -> IO ()
checkConfig cfg proc = do
  let mLogFile = fmap unConfigPath $ appLastRebootTimeFilePath cfg
  case mLogFile of
    Just logFile -> do
      isExist <- doesFileExist logFile
      if isExist
        then proc
        else error $ logFileNonExistMsg logFile
    Nothing -> proc
  where
    logFileNonExistMsg file = [qc|Error: Log file {file} does not exist.
Check config field lastRebootTimeFilePath|]

runTcpListener :: Config -> TimeVar -> IO ()
runTcpListener cfg tv = void $ forkIO $ do
  serve host port $ \(connectionSocket, remoteAddr) -> do
    resp <- recv connectionSocket 1
    when (isJust resp) $ do
      updateTime tv
  where
    serverCfg = appServer cfg
    host = Host $ T.unpack $ serverHost serverCfg
    port = show $ serverPort serverCfg


runRebootWorker :: Config -> TimeVar -> IO ()
runRebootWorker cfg tv = runTimeCheckWorker cfg tv (reboot cfg)

runTimeCheckWorker :: Config -> TimeVar -> (IO ()) -> IO ()
runTimeCheckWorker cfg tv proc = void $ forkIO $ loop
  where
    totalDuration = appDuration cfg

    loop = do
      sleep totalDuration
      isOk <- checkTime tv
      if isOk
        then loop
        else do
          proc
          loop

    checkTime tv =  fmap ( < totalDuration) $ timePassed tv

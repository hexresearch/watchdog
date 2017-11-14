module Signal(
    parseSignal
  , initSignalHandler
) where

import Control.Monad

import Data.Char
import System.Posix.Signals

import qualified Data.List as L

initSignalHandler :: Signal -> IO () -> IO ()
initSignalHandler sig proc =
  void $ installHandler sig (Catch proc) Nothing

parseSignal :: String -> Maybe Signal
parseSignal str =
  case removeSigPrefix $ fmap toLower str of
    "abrt" -> Just sigABRT
    "alrm" -> Just sigALRM
    "fpe" -> Just sigFPE
    "hup" -> Just sigHUP
    "ill" -> Just sigILL
    "int" -> Just sigINT
    "kill" -> Just sigKILL
    "pipe" -> Just sigPIPE
    "quit" -> Just sigQUIT
    "segv" -> Just sigSEGV
    "term" -> Just sigTERM
    "usr1" -> Just sigUSR1
    "usr2" -> Just sigUSR2
    "chld" -> Just sigCHLD
    "cont" -> Just sigCONT
    "stop" -> Just sigSTOP
    "tstp" -> Just sigTSTP
    "ttin" -> Just sigTTIN
    "ttou" -> Just sigTTOU
    "bus" -> Just sigBUS
    "poll" -> Just sigPOLL
    "prof" -> Just sigPROF
    "sys" -> Just sigSYS
    "trap" -> Just sigTRAP
    "urg" -> Just sigURG
    "vtalrm" -> Just sigVTALRM
    "xcpu" -> Just sigXCPU
    "xfsz" -> Just sigXFSZ
    _ -> Nothing
  where
    removeSigPrefix x = maybe x id $ L.stripPrefix "sig" x


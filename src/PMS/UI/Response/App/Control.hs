{-# LANGUAGE LambdaCase #-}

module PMS.UI.Response.App.Control where

import System.IO
import qualified Control.Exception.Safe as E
import System.Log.FastLogger
import Data.Default

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.UI.Response.DM.Type
import PMS.UI.Response.DM.Constant
import PMS.UI.Response.DS.Utility
import PMS.UI.Response.DS.Core

-- |
--
run :: DM.DomainContext ()
run dat = do
  hPutStrLn stderr "[INFO] PMS.UI.Response.App.Control.run called."
  runWithAppData def dat

-- |
--
runWithAppData :: AppData -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

-- |
--
runWithLogger :: (TimedFastLogger, IO ()) -> AppData -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runApp domDat appDat logger app
    >>= \case
      Right _ -> return ()
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] PMS.UI.Response.App.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.UI.Response.App.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.UI.Response.App.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"

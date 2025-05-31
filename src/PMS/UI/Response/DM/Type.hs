{-# LANGUAGE TemplateHaskell #-}

module  PMS.UI.Response.DM.Type where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Data.Default
import System.IO
import Control.Lens

import qualified PMS.Domain.Model.DM.Type as DM


data AppData = AppData {
               _outputHandleAppData :: Handle  
             }

makeLenses ''AppData

instance Default AppData where
  def = AppData {
          _outputHandleAppData = stdout
        }

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))

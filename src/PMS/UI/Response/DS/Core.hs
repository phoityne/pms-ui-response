{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module PMS.UI.Response.DS.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as B
import Data.Conduit
import qualified Data.Text as T
import Data.Aeson
import Control.Monad.Except

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.UI.Response.DM.Type


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| work .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.McpResponse AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.McpResponse
    go = do
      queue <- view DM.responseQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT DM.McpResponse DM.JsonRpcResponse AppContext ()
work = await >>= \case
  Just reqBS -> flip catchError errHdl $ do
    lift (go reqBS) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work

  where
    errHdl :: String -> ConduitT DM.McpResponse DM.JsonRpcResponse AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: parse error. skip. " ++ msg
      work

    go :: DM.McpResponse -> AppContext DM.JsonRpcResponse
    go res = do
      jsonRes <- mcp2json res
      $logDebugS DM._LOGTAG $ T.pack $ "work: response: " ++ show jsonRes
      return jsonRes

-- |
--
mcp2json :: DM.McpResponse -> AppContext DM.JsonRpcResponse
mcp2json (DM.McpInitializeResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpInitializeResponseData
      result = encode $ dat^.DM.resultMcpInitializeResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpToolsListResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpToolsListResponseData
      result = encode $ dat^.DM.resultMcpToolsListResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpToolsCallResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpToolsCallResponseData
      result = encode $ dat^.DM.resultMcpToolsCallResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res  
mcp2json (DM.McpPromptsListResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpPromptsListResponseData
      result = encode $ dat^.DM.resultMcpPromptsListResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpPromptsGetResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpPromptsGetResponseData
      result = encode $ dat^.DM.resultMcpPromptsGetResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpResourcesTemplatesListResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpResourcesTemplatesListResponseData
      result = encode $ dat^.DM.resultMcpResourcesTemplatesListResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpResourcesListResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpResourcesListResponseData
      result = encode $ dat^.DM.resultMcpResourcesListResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpResourcesReadResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpResourcesReadResponseData
      result = encode $ dat^.DM.resultMcpResourcesReadResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res
mcp2json (DM.McpCompleteResponse dat) = do
  let json = DM.defaultJsonRpcResponse $ dat^.DM.jsonrpcMcpCompleteResponseData
      result = encode $ dat^.DM.resultMcpCompleteResponseData
      res = json {DM._resultJsonRpcResponse = DM.RawJsonByteString result}
  return res

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT DM.JsonRpcResponse Void AppContext ()
sink = await >>= \case
  Just req -> lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    go :: DM.JsonRpcResponse -> AppContext ()
    go res = do
      hdl <- view outputHandleAppData <$> ask
      let bs = encode res
      $logDebugS DM._LOGTAG $ T.pack $ "sink: response bs: " ++ DM.lbs2str bs

      liftIO $ B.hPutStr hdl bs
      liftIO $ B.hPutStr hdl "\n"
      liftIO $ hFlush hdl

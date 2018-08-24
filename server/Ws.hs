{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Ws where

import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Time.LocalTime

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Base64 as BSB

import GHC.Float
import Text.Printf

import Text.Pretty.Simple
import Network.WebSockets

import GHC.Generics
import System.Timeout

import Data.TTN
import Data.TTN.Client
import Data.TTN.Client.Util
import qualified Data.Cayene as CLPP
import Data.Binary.Get

import qualified Common as Common

debug = False

server :: Int -> TVar Common.Model -> IO ()
server port model = do
  c <- atomically $ newBroadcastTChan
  forkIO $ ttnClient c
  forkIO $ bufferSome 10 c model

  putStrLn $ "[ws] Running on port " ++ (show port)

  runServer "0.0.0.0" port (handleConnection c model)

bufferSome num chan model = do
  dchan <- atomically $ dupTChan chan
  forever $ do
    evt <- atomically $ readTChan dchan
    now <- getZonedTime
    case evt of
      Event typ uplink -> do
        case typ of
          Up -> do
            case uplinkPayloadRaw uplink of
              Nothing -> return ()
              Just payload -> do
                let wrappedEvt = Common.Evt evt now (tryDecode payload)
                atomically $ modifyTVar model $ \x -> x { Common.ttnEvents = take num (wrappedEvt:(Common.ttnEvents x)) }
          _ -> do
            let wrappedEvt = Common.Evt evt now []
            atomically $ modifyTVar model $ \x -> x { Common.ttnEvents = take num (wrappedEvt:(Common.ttnEvents x)) }
      _ -> return ()

    cm <- atomically $ readTVar model
    return ()

handleConnection chan model pending = do
  putStrLn "Got new websocket connection"
  connection <- acceptRequest pending
  dchan <- atomically $ dupTChan chan


  msg <- receiveDataMessage connection
  case msg of
    Binary _ -> return ()
    Text t _ -> do
      case decode t of
        Nothing -> return ()
        Just (Common.Cold True) -> do
          -- if this is a cold boot send language
          let lang = lookup "Accept-Language" (requestHeaders $ pendingRequest pending)
          case lang of
            Nothing -> return ()
            Just l -> do
              case knownLang (T.pack . BS.unpack $ l) of
                Nothing -> return ()
                Just x -> sendTextData connection $ encode $ Common.SetLang x

          -- send backlog
          cm <- atomically $ readTVar model
          forM_ (reverse $ Common.ttnEvents cm) $ \evt -> do
            sendTextData connection $ encode $ Common.NewEvent evt
            threadDelay $ round (1 * 1000000)
        Just _ -> return ()


  atomically $ modifyTVar model $ \x -> x { Common.clients = (Common.clients x) + 1 }

  -- spawn data feeder
  void $ forkIO $ forever $ do
    evt <- atomically $ readTChan dchan
    when debug $ sendTextData connection $ encode $ Common.Debug ((T.pack $ show evt) :: T.Text)

    now <- getZonedTime

    sendTextData connection $ encode $ Common.NewEvent $ Common.Evt evt now (decodeUplink evt)
    -- make it more dramatic
    threadDelay $ round (1 * 1000000)

  -- spawn pinger thread
  let pinger = do
        -- send num clients
        cm <- atomically $ readTVar model
        sendTextData connection $ encode $ Common.UpdateClients $ Common.clients cm

        -- send ping
        sendTextData connection $ encode Common.Ping

        pongMsg <- timeout (20 * 1000000) $ receiveDataMessage connection
        when debug $ print pongMsg
        case pongMsg of
          Nothing -> do
            when debug $ print "No pong"
            return ()
          Just _ -> do
            threadDelay (60 * 1000000)
            pinger

  let disconnect = do
        atomically $ modifyTVar model $ \x -> x { Common.clients = (Common.clients x) - 1 }

  finally pinger disconnect

langs = ["sk", "cs"]
knownLang header = case proc of
    [] -> Nothing
    (x:xs) -> Just x
  where
    proc = catMaybes $ map test langs
    test lang = if lang `T.isInfixOf` header then Just lang else Nothing

decodeUplink (Event Up Uplink { uplinkPayloadRaw = Just payload }) = tryDecode payload
decodeUplink _ = []

tryDecode :: T.Text -> [Common.Decoded]
tryDecode x = rights $ map (\f -> f . unbase64 $ x )
  [ decodeTH
  , decodeCLPP
  ]

decodeTH x = case runGetOrFail desTH x of
  Left (_, _, err) -> Left err
  Right (_, _, a)  -> Right a

desTH :: Get Common.Decoded
desTH = do
  t <- getFloathost
  h <- getFloathost
  return $ Common.TempHumidity t h

decodeCLPP :: BSL.ByteString -> Either String Common.Decoded
decodeCLPP x = case CLPP.decodeMany x of
  [] -> Left "no CLPP data decoded"
  x  -> Right $ Common.Cayene x

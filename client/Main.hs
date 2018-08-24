{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Proxy

import Miso
import Miso.String
import qualified Data.Text as T

import Common

wsURL = URL "ws://127.0.0.1:8000"

main :: IO ()
main = miso $ \currentURI -> App
        { model = uriModel currentURI
        , view = viewModel
        , ..
        }
    where
      initialAction = Greet
      mountPoint = Nothing
      update = updateModel
      events = defaultEvents
      subs = [
          uriSub HandleURI
        , websocketSub wsuri protocols HandleWebSocket
        ]
      wsuri = wsURL
      protocols = Protocols [ ]
      viewModel m =
        case runRoute (Proxy :: Proxy ClientRoutes) handlers uri m of
          Left _ -> the404 m
          Right v -> v

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (HandleURI u) m = noEff m { uri = u }
updateModel (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel (ChangeLang l) m = noEff m { lang = T.pack $ unpack l }
updateModel (ChangeTimeFormat tf) m = noEff m { timeFormat = tf }
updateModel ToggleSettings m = noEff m { settings = not $ settings m }
updateModel (HandleWebSocket WebSocketOpen) model = model { connected = True } <# do
  send (Cold (Prelude.length (ttnEvents model) == 0))
  pure NoOp
updateModel (HandleWebSocket (WebSocketClose _ _ _)) model = noEff model { connected = False }
updateModel (HandleWebSocket (WebSocketMessage Ping)) model = model <# do
   send Pong
   pure NoOp
updateModel (HandleWebSocket (WebSocketMessage (NewEvent e))) model
  = noEff model { ttnEvents = Prelude.take (displayLimit model) (e:(ttnEvents model))
                , eventCount = 1 + (eventCount model) }
updateModel (HandleWebSocket (WebSocketMessage (UpdateClients x))) model
   = noEff model { clients = x }
updateModel (HandleWebSocket (WebSocketMessage (SetLang x))) model
   = noEff model { lang = x }
updateModel DumpModel m = m <# do
  putStrLn (show m) >> pure NoOp
updateModel x m = m <# do
  putStrLn (show x) >> pure NoOp

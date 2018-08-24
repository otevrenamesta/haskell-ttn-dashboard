{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Aeson
import GHC.Generics

import Miso
import Miso.String

import Common


--newtype Message = Message MisoString
--  deriving (Eq, Show, Generic, Monoid)

data Message = Ping
  | Debug MisoString
  deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

-- | Type synonym for an application model
data Model = Model {
    msg :: Message
  , debug :: MisoString
  } deriving (Show, Eq)

-- | Sum type for application events
data Action
  = AddOne
  | NoOp
  | SayHelloWorld
  | HandleWebSocket (WebSocket Message)
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = Model Ping mempty
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs = [ websocketSub uri protocols HandleWebSocket ]
    uri = URL "ws://127.0.0.1:8000"
    protocols = Protocols [ ]
    mountPoint = Nothing

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff m -- (m + 1)
updateModel NoOp m = noEff m
updateModel (HandleWebSocket (WebSocketMessage (Debug m))) model
   = noEff model { debug = m <> (debug model) }
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World" >> pure NoOp
updateModel x m = m <# do
  putStrLn (show x) >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel Model{..} = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms debug)
 ]

template :: View Action -> Model -> View Action
template content Model{..} =
  div_ [ ] [ content ]

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
module Main where

import           Common
import           Data.Aeson
import           Data.Proxy
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           GHC.Generics
import qualified Lucid                                as L
import           Lucid.Base
import           Network.HTTP.Types hiding (Header)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Application.Static
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Servant.Server.Internal
import qualified System.IO                            as IO
import           System.Environment

import           Miso
import           Miso.String

import qualified Ws
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

main :: IO ()
main = do
  (port, wsport) <- getPorts
  m <- atomically $ newTVar $ emptyModel
  forkIO $ Ws.server wsport m
  IO.hPutStrLn IO.stderr $ "Running on port " ++ (show port)
  run port $ logStdout (compress $ app m)
    where
      compress = gzip def { gzipFiles = GzipCompress }
      getPorts = do
        args <- getArgs
        case args of
          [p, wsp] | [(port, _)] <- reads p , [(wsport, _)] <- reads wsp -> return (port, wsport)
          _ -> return (3002, 8000)

app :: TVar Model -> Application
app m = serve (Proxy @ API) (static :<|> (serverHandlers m) :<|> pure appManifest :<|> Tagged handle404)
  where
    static = serveDirectoryWith (defaultWebAppSettings "static")

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

-- | API type
type API = ("static" :> Raw)
  :<|> ServerRoutes
  :<|> ("manifest.json" :> Get '[JSON] Manifest)
  :<|> Raw

data Manifest
  = Manifest
  { name :: Text
  , short_name :: Text
  , start_url :: Text
  , display :: Text
  , theme_color :: Text
  , description :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Manifest

appManifest :: Manifest
appManifest =
  Manifest { name = "LoRaWAN Demo"
           , short_name = "TTNOM"
           , start_url = "."
           , display = "standalone"
           , theme_color = "SpringGreen"
           , description = "LoRaWAN Live Demo Dashboard"
           }

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
      renderBS $ toHtml $ Wrapper $ the404 $ uriModel goHome

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) = do
      L.doctype_
      L.html_ [ L.lang_ "en" ] $ do
        L.head_ $ do
          L.title_ "LoRaWAN Demo"
          L.link_ [ L.rel_ "manifest"
                  , L.href_ "/manifest.json"
                  ]
          L.meta_ [ L.charset_ "utf-8" ]
          L.meta_ [ L.name_ "theme-color", L.content_ "SpringGreen" ]
          L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]
          L.meta_ [ L.name_ "viewport"
                  , L.content_ "width=device-width, initial-scale=1"
                  ]
          L.meta_ [ L.name_ "description"
                  , L.content_ "Live sensor data"
                  ]
          cssRef "static/fa/css/all.css"
          cssRef "static/bootstrap.css"
          cssRef "static/animate.css"
          cssRef "static/site.css"
          jsRef "static/all.js"
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

serverHandlers ::
       TVar Model
  ->   Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
serverHandlers m = aboutHandler
  :<|> docsHandler
  :<|> communityHandler
  :<|> homeHandler
     where
       send f u = pure $ Wrapper $ f $ uriModel u
       {-
        -- this can be used to render with serverside model, breaks dom diffing
       send f u = do
         cm <- liftIO $ atomically $ readTVar m
         return $ Wrapper $ f $ cm { uri = u }
       -}
       homeHandler = send home goHome
       aboutHandler = send about goAbout
       docsHandler  = send docs goDocs
       communityHandler = send community goCommunity


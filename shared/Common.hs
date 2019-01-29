{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
module Common where

import           Data.Bool
import qualified Data.Map    as M
import           Data.Monoid
import           Data.Proxy
import           Servant.API
import           Network.URI (nullURI, URIAuth)
import           Servant.Utils.Links

import           Miso
import           Miso.String   hiding (head, any, filter, map, minimum, maximum, zip, foldl, null)
import           Miso.Svg      hiding (height_, id_, style_, width_, a_, title_)

import           Data.Aeson hiding (defaultOptions)
import qualified Data.Text as T
import qualified Data.TTN as TTN


import GHC.Generics

import Gateways
import Messages

import Text.Shakespeare.I18N (Lang)

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock.POSIX

import qualified Data.Cayene as CLPP

data Decoded =
    TempHumidity Float Float
  | Cayene [CLPP.Reading]
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CLPP.Sensor
instance FromJSON CLPP.Sensor
instance ToJSON Decoded
instance FromJSON Decoded

data Evt = Evt TTN.Event ZonedTime [Decoded]
  deriving (Eq, Show, Generic)

instance ToJSON Evt
instance FromJSON Evt

-- Message is used for webSocket protocol
data Message = Ping
  | Pong
  | Cold Bool
  | Debug MisoString
  | NewEvent Evt
  | UpdateClients Int
  | SetLang Lang
  deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

-- | Model
data Model = Model
  { uri :: URI
  , clients :: Int
  , connected :: Bool
  , settings :: Bool
  , timeFormat :: MisoString
  , ttnEvents :: [Evt]
  , eventCount :: Int
  , displayLimit :: Int
  , lang :: Lang
  } deriving (Eq, Show, Generic)

emptyModel = Model
  { uri = nullURI
  , clients = 0
  , connected = False
  , settings = False
  , timeFormat = "%X"  -- "%F %X"
  , ttnEvents = []
  , eventCount = 0
  , displayLimit = 100
  , lang = "en"
  }

uriModel x = emptyModel { uri = x }

-- | Event Actions
data Action
  = Alert
  | Greet
  | DumpModel
  | ToggleBounce
  | ToggleSettings
  | ChangeLang MisoString
  | ChangeTimeFormat MisoString
  | ChangeURI URI
  | HandleURI URI
  | HandleWebSocket (WebSocket Message)
  | NoOp
  deriving (Show, Eq)

-- | Router
type ClientRoutes = About
  :<|> Docs
  :<|> Community
  :<|> Home

-- | Handlers
handlers = about
  :<|> docs
  :<|> community
  :<|> home

-- | Client Routes
type About  = "about" :> View Action
type Docs      = "docs" :> View Action
type Community = "community" :> View Action
type Home      = View Action

-- | Views
community :: Model -> View Action
community m@Model{..} = template v m
  where
    v = div_ [] [ text "Community" ]

docs :: Model -> View Action
docs m@Model{..} = template v m
  where
    v = div_ [] [
          h2_ [] [ tr lang MsgDocs ]
        , h3_ [] [ tr lang MsgEnglishResources ]
        , ul_ [] [
            li_ [] [ a_ [ href_ "https://www.thethingsnetwork.org/docs/lorawan/" ] [ text "LoRaWAN" ] ]
          , li_ [] [ a_ [ href_ "https://en.wikipedia.org/wiki/LoRa" ] [ text "LoRa (wikipedia)" ] ]
          , li_ [] [ a_ [ href_ "https://www.thethingsnetwork.org/" ] [ tr lang MsgTTN ] ]
          , li_ [] [ a_ [ href_ "https://www.thethingsnetwork.org/docs/applications/mqtt/api.html" ] [ text "TTN MQTT API" ] ]
          , li_ [] [ a_ [ href_ "https://wiki.base48.cz/TTN" ] [ text "base48.cz TTN wiki page"  ] ]
          , li_ [] [ a_ [ href_ "https://www.thethingsnetwork.org/community/Brno/" ] [ text "TTN Brno community" ] ]
          , li_ [] [ a_ [ href_ "https://ttnmapper.org/" ] [ text "TTN Mapper" ] ]
          ]
        , h3_ [] [ tr lang MsgCzechResources ]
        , ul_ [] [
            li_ [] [ a_ [ href_ "https://blog.frantovo.cz/c/366/The%20Things%20Network%20%E2%80%93%20LoRaWAN%20%E2%80%93%C2%A0IoT" ] [ text "The Things Network – LoRaWAN – IoT (blogpost)" ] ]
          , li_ [] [ a_ [ href_ "https://cs.wikipedia.org/wiki/LoRa" ] [ text "LoRa (wikipedia)" ] ]
        ]
      ]

about :: Model -> View Action
about m@Model{..} = template v m
  where
    v = div_ [] [
          h2_ [] [ tr lang MsgDocs ]
        , p_ [] [
            tr lang MsgAboutBy
          , sep
          , a_ [ href_ "https://base48.cz" ] [ text "base48.cz hackerspace" ]
          , sep
          , tr lang MsgAboutInCoOp
          , ul_ [] [
              li_ [] [ a_ [ href_ "http://otevrenamesta.cz/" ] [ text "Otevřená města" ] ]
            , li_ [] [ a_ [ href_ "https://www.nmnm.cz/" ] [ text "Nové Město na Moravě" ] ]
          , p_ [] [
              a_ [ href_ "http://otevrenamesta.cz/" ] [ img_ [ src_ "static/img/om.svg", width_ "300", alt_ "Otevřená města" ] ]
            , a_ [ href_ "https://www.nmnm.cz/" ] [ img_ [ src_ "static/img/nmnm.png", width_ "100", alt_ "Nové Město na Moravě" ] ]
            ]
          ]
        ]
        , p_ [] [
            tr lang MsgAboutPowered
          , sep
          , a_ [ href_ "https://www.haskell.org/" ] [ text "Haskell" ]
          , sep
          , tr lang MsgAboutPowered2
          , sep
          , a_ [ href_ "https://haskell-miso.org/" ] [ tr lang MsgAboutMiso ]
          ]
        , p_ [] [
            tr lang MsgAboutSensor
          , sep
          , a_ [ href_ "https://ivorylang.org/" ] [ tr lang MsgAboutIvoryTower ]
        ]
        , p_ [] [
            tr lang MsgAboutGateways
          , sep
          , a_ [ href_ "https://wiki.base48.cz/TTN" ] [ text "base48.cz hackerspace wiki" ]
        ]
        , p_ [] [
            tr lang MsgAboutHosted
          , sep
          , a_ [ href_ "https://vpsfree.cz" ] [ span_ [ style_ $ M.fromList [(pack "color", pack "#f6914b")] ] [ text "vps" ]
                                              , span_ [ style_ $ M.fromList [(pack "color", pack "#000")] ] [ text "Free.cz" ]
                                              ]
        ]
        , p_ [] [
            a_ [ href_ "https://base48.cz/" ] [
              img_ [
                  src_ "static/img/base.svg"
                , style_ $ M.fromList [(pack "padding-left", pack "4em")]
                , width_ "200"
                , alt_ "base48"
                ]
              ]
          , a_ [ href_ "https://vpsfree.cz/" ] [
              img_ [
                  src_ "static/img/vpsf.png"
                , style_ $ M.fromList [(pack "margin-left", pack "5em")]
                , width_ "200"
                , alt_ "vpsFree.cz" ]
              ]
        ]
        , h3_ [] [ tr lang MsgAboutSources ]
        , p_ [] [
          ul_ [] [
              li_ [] [ a_ [ href_ "https://github.com/otevrenamesta/haskell-ttn-dashboard" ] [ text "haskell-ttn-dashboard" ], sep, tr lang MsgAboutThisApp ]
            , li_ [] [ a_ [ href_ "https://github.com/sorki/data-ttn"   ] [ text "data-ttn" ] ]
            , li_ [] [ a_ [ href_ "https://github.com/sorki/cayene-lpp" ] [ text "cayene-lpp" ] ]
            , li_ [] [ a_ [ href_ "https://github.com/sorki/ttn-client" ] [ text "ttn-client" ] ]
          ]
        ]
      ]

home :: Model -> View Action
home m@Model{..} = template v m
  where
    v = div_ [] [
          h1_ [] [ text "LoRaWAN Demo" ]
        , p_ [] [ tr lang MsgHomeIntro, sep,  a_ [ href_ "https://www.thethingsnetwork.org/" ] [ tr lang MsgTTN ], text "." ]
        , pTr MsgHomeSensor
        , pTr MsgHomeAPI
      ]
    pTr x = p_ [] [ tr lang x ]

langs = [
    ("cs", MsgLangCzech)
  , ("sk", MsgLangSlovak)
  , ("en", MsgLangEnglish)
  ]

tfs = [
    ("%X", MsgTFTime)
  , ("%F %X", MsgTFDateTime)
  ]

template :: View Action -> Model -> View Action
template content m@Model{..} = div_ [ id_ "template" ] [
    div_ [] [ newNav m ]
  , div_ [  class_ "container" ] [
      div_ [ class_ "row" ] [
          div_ [ class_ "col" ] [
            content
          ]
        , div_ [ class_ "col" ] [
            iff (connected && (not $ null ttnEvents)) $ div_ [ class_ "heroth animated fadeIn" ] [
              h2_ [] [ tr lang MsgLastReadings ]
              ,  renderTH m
            ]
          ]
        , iff settings $ div_ [ class_ "col" ] [
            h2_ [] [ tr lang MsgSettings ]
          , form_ [] [
                div_ [ class_ "form-group" ] [
                    label_ [ for_ "language" ] [ tr lang MsgLanguage ]
                  , select_ [ class_ "language form-control", onChange ChangeLang ] (langOptions lang)
                ]
              , div_ [ class_ "form-group" ] [
                    label_ [ for_ "timeFormat" ] [ tr lang MsgTimeFormat ]
                  , select_ [ class_ "timeFormat form-control", onChange ChangeTimeFormat ] (tfOptions lang timeFormat)
                ]
              , button_ [ onClick ToggleSettings, type_ "button", class_ "btn btn-success" ] [ i_ [ class_ "fa fa-check" ] [ ] ]
            ]
          ]
      ]
    , div_ [ class_ "row" ] [
        renderEvents m
      ]
    ]
  ]
  where
    langOptions currentLang = map (mkOption currentLang) langs
    mkOption current (short, msg) = option_ [ selected_ ((pack $ T.unpack $ current) == short), value_ short ] [ tr current msg ]

    tfOptions lang current = map (mkTFOption lang current) tfs
    mkTFOption lang current (short, msg) = option_ [ selected_ (current == short), value_ short ] [ tr lang msg ]

iff True what = what
iff False _ = span_ [] []

tempHumOnly :: Evt -> Bool
tempHumOnly (Evt (TTN.Event TTN.Up u@TTN.Uplink{..}) _ decoded) = any isTH decoded
tempHumOnly _ = False

isTH :: Decoded -> Bool
isTH (TempHumidity _ _) = True
isTH _ = False

getTemp (TempHumidity t h) = t
getHumi (TempHumidity t h) = h

connectionStatus :: Lang -> Bool -> View Action
connectionStatus lang st = div_ [ id_ "connectionStatus", class_ (classByStatus st) ] [
    i_ [ class_ "fa fa-signal" ] [ ]
  , span_ [ class_ "textual" ] [ tr lang (bySt st) ]
  ]
  where bySt True = MsgConnected
        bySt False = MsgDisconnected

classByStatus True = "connected"
classByStatus False = "disconnected"

numClients :: Lang -> Int -> View Action
numClients lang c = div_ [ id_ "numClients" ] [
    i_ [ class_ "fa fa-user-astronaut" ] [ ]
  , span_ [ class_ "textual" ] [ tr lang (MsgConnectedClients c) ]
  ]

graph :: [Int] -> View Action
graph ps = div_ [ id_ "graph" ] (map point ps)

point :: Int -> View Action
point p = p_ [ id_ ("blah" <> (pack $ show p)) ] [ text (pack $ show p) ]

renderEvents m@Model{..} = table_ [ id_ "eventTable", class_ "table table-striped" ] [
    thead_ [ class_ "thead-dark" ] [
      tr_ [] [
        th_ [] [ tr lang MsgDirection ]
      , th_ [] [ tr lang MsgTime ]
      , th_ [] [ tr lang MsgDevice ]
      , th_ [] [ tr lang MsgApplication ]
      , th_ [] [ tr lang MsgCounter ]
      , th_ [] [ tr lang MsgPayload ]
      ]
    ]
    --, tbody_ [] (foldl (\accum evt -> accum ++ [event m evt, eventDetail m evt]) [] ttnEvents)
    , tbody_ [] (foldl (\accum evt -> case accum of
                           [] -> [event m evt True, eventDetail m evt True]
                           _ -> accum ++ [event m evt False, eventDetail m evt False]
                           ) [] ttnEvents)
  ]

event :: Model -> Evt -> Bool -> View Action
event m@Model{..} (Evt (TTN.Event typ u@TTN.Uplink{..}) srvtime decoded) firstRow = tr_ [ class_ ((fadeClass firstRow) <> (pack $ classByTyp typ)) ] [
    td_ [] [ renderTyp m typ ]
  , td_ [] [ renderTimes uplinkMetadata srvtime timeFormat ]
  , td_ [] [ renderDevId m u ]
  , td_ [] [ renderAppId m u ]
  , td_ [] [ renderCounters uplinkCounter uplinkConfig ]
  , td_ [] (renderDecoded m decoded)
  ]
event m (Evt (TTN.ClientError err) _ _) _ = p_ [] [ text $ pack err ]

--fadeClass True = "animated bounceIn "
--fadeClass False = " "
fadeClass _ = ""

eventDetail :: Model -> Evt -> Bool -> View Action
eventDetail m (Evt (TTN.ClientError err) _ _) _ = blank
eventDetail m@Model{..} (Evt (TTN.Event typ u@TTN.Uplink{..}) srvtime decoded) firstRow = tr_ [  class_ ((fadeClass firstRow) <> (pack $ classByTyp typ)) ] [
    td_ [] []
  , td_ [ colspan_ "2" ] [
      ul_ [ class_ "fa-ul" ] [
          faLi "signature" renderFreq
        , faLi "database" renderDataRate
        -- , faLi "clock" renderAirTime
      ]
    ]
  , td_ [] []
  , td_ [ colspan_ "2" ] [
      iff (hasGws uplinkMetadata) $ div_ [ class_ "gateways" ] [
        --  span_ [] [ tr lang MsgGateways ]
          ul_ [ class_ "fa-ul" ] (renderGateways lang (getGws uplinkMetadata))
        ]
    , renderUplinkGateway lang uplinkGatewayId
    ]
  ]
  where faLi cls fn = li_ [] [
            span_ [ class_ "fa-li" ] [
              i_ [ class_ ("fa fa-fw fa-" <> cls) ] [ ]
            ]
          , span_ [ class_ "textual" ] [ (fn m u) ]
          ]
        hasGws Nothing = False
        hasGws (Just TTN.Metadata{..}) = metadataGateways /= []
        getGws Nothing = []
        getGws (Just TTN.Metadata{..}) = metadataGateways

blank = text ""
sep = span_ [] [ text " " ]

renderGateways lang gws = map (renderGateway lang) gws

renderGateway lang TTN.GatewaysElt{..} = faLi "broadcast-tower" $ div_ [] [
    span_ [] [ maybeGwName gatewaysEltGtwId ]
  , ul_ [ class_ "fa-ul" ] [
      faLi (classBySignal gatewaysEltRSSI gatewaysEltSNR) (tr lang $ MsgRSSISNR gatewaysEltRSSI gatewaysEltSNR)
    , faLi "rss" (tr lang $ MsgChannel gatewaysEltChannel)
    ]
  ]
  where
    maybeGwName Nothing = tr lang MsgUnknownGw
    maybeGwName (Just x) = case M.lookup (T.unpack x) gatewayMapping of
      Nothing -> fromText x
      Just (gwmsg, lat, lon) -> a_ [ href_ (mapperURL lat lon) ] [ tr lang gwmsg ]

-- eui-b827ebfffec6e5d0 -> B827EBFFFEC6E5D0
cvtEUI = T.toUpper . (T.drop 4)

mapperURL lat lon = "https://ttnmapper.org/?lat=" <> (pack $ show lat) <> "&lon=" <> (pack $ show lon) <> "&zoom=15&type=radar&layer=mapnik"

renderUplinkGateway lang Nothing = blank
renderUplinkGateway lang maybeEUI = ul_ [ class_ "fa-ul" ] [
    faLi "broadcast-tower" $ div_ [] [
      span_ [] [ maybeGwName maybeEUI ]
    ]
  ]
  where
    maybeGwName Nothing = tr lang MsgUnknownGw
    maybeGwName (Just x) = case M.lookup (T.unpack x) gatewayMapping of
      Nothing -> fromText x
      Just (gwmsg, lat, lon) -> a_ [ href_ (mapperURL lat lon) ] [ tr lang gwmsg ]

faLi cls txt = li_ [] [
    span_ [ class_ "fa-li" ] [
      i_ [ class_ ("fa fa-fw fa-" <> cls) ] [ ]
    ]
  , span_ [ class_ "textual" ] [ txt ]
  ]

renderCounters (Just c) _ = renderInt c
renderCounters Nothing (Just TTN.Config { configCounter = Just c }) = renderInt c
renderCounters _ _ = blank

renderTimes (Just TTN.Metadata { metadataTime = Just ttnTime }) _ f = text $ pack $ renderTime (TTN.unwrap ttnTime) f
renderTimes Nothing srv f = text $ pack $ renderTime srv f

cet :: TimeZone
cet = hoursToTimeZone 1

cest :: TimeZone
cest = hoursToTimeZone 2

cvt t = utcToLocalTime tz $ zonedTimeToUTC t
  where tz = cest

renderTime t f = formatTime defaultTimeLocale (unpack f) (cvt t)

zonedToTimestamp :: ZonedTime -> Float
zonedToTimestamp = read . formatTime defaultTimeLocale "%s" . zonedTimeToUTC

renderPayload Nothing  = text ""
renderPayload (Just p) = text $ pack $ T.unpack p

renderDecoded m@Model{..} xs = map (renderData lang) xs

renderData lang (TempHumidity t h) = ul_ [ class_ "data fa-ul" ] [
    li_ [] [
        span_ [ class_ "fa-li" ] [
          i_ [ class_ ("fa fa-fw " <> (pack $ classByTemp t)), trTitle lang (MsgTemperature t)] [ ]
        ]
      , span_ [ class_ "textual" ] [ tr lang (MsgTemperature t) ]
    ]
  , li_ [] [
        span_ [ class_ "fa-li" ] [
          i_ [ class_"fa fa-tint fa-fw", trTitle lang (MsgHumidity h)] [ ]
        ]
      , span_ [ class_ "textual" ] [ tr lang (MsgHumidity h) ]
    ]
  ]
renderData lang (Cayene readings) = div_ [] (map (renderCayene lang) readings)

renderCayene lang (chan, sensor) = span_ [] []

renderTH m@Model{..} = case filter tempHumOnly ttnEvents of
  [] -> blank
  ((Evt _ _ decoded):_) -> renderData lang $ head $ filter isTH decoded

renderTyp m@Model{..} TTN.Activation = div_ [] [
    i_ [ class_"fa fa-toggle-on fa-fw", title_ "Activation"] [ ]
  , span_ [ class_ "textual" ] [ tr lang MsgActivation ]
  ]
renderTyp m@Model{..} TTN.Up = div_ [] [
    i_ [ class_"fa fa-upload fa-fw", title_ "Uplink"] [ ]
  , span_ [ class_ "textual" ] [ tr lang MsgUplink ]
  ]
renderTyp m@Model{..} TTN.DownSent = div_ [] [
    i_ [ class_"fa fa-download fa-fw", title_ "Downlink" ] [ ]
  , span_ [ class_ "textual" ] [ tr lang MsgDownLink ]
  ]
renderTyp m@Model{..} t = div_ [] [
    i_ [ class_"fa fa-question-circle fa-fw", title_ "Unknown" ] [ ]
  , span_ [ class_ "textual" ] [ text $ pack $ show t ]
  ]

classByTyp TTN.Activation = "activation"
classByTyp TTN.Up = "uplink"
classByTyp TTN.DownSent =  "down-sent"
classByTyp t = "unknown"

classByTemp x | x < 0             = "fa-thermometer-empty"
classByTemp x | x >= 0 && x < 10  = "fa-thermometer-quarter"
classByTemp x | x >= 10 && x < 20 = "fa-thermometer-half"
classByTemp x | x >= 20 && x < 30 = "fa-thermometer-three-quarters"
classByTemp x | x >= 30           = "fa-thermometer-full"

-- ideally this should consider snr as well
classBySignal rssi snr | rssi < -110 = "volume-off  signal-red"
classBySignal rssi snr | rssi < -100 = "volume-down signal-yellow"
classBySignal rssi snr | otherwise   = "volume-up   signal-green"

choose :: Model -> (Model -> a -> View action) -> Maybe a -> Maybe a -> View action
choose m fn (Just x) _ = fn m x
choose m fn _ (Just y) = fn m y
choose m fn _ _ = blank

renderDevId m u = choose m render (TTN.uplinkDevId u) (fromMessage $ TTN.uplinkMessage u)
  where fromMessage Nothing = Nothing
        fromMessage (Just m) = Just $ TTN.messageDevId m
        render m x = fromText x

renderAppId m u = choose m render (TTN.uplinkAppId u) (fromMessage $ TTN.uplinkMessage u)
  where fromMessage Nothing = Nothing
        fromMessage (Just m) = Just $ TTN.messageAppId m
        render m x = fromText x

renderFreq m u = choose m render (fromMaybe (TTN.uplinkMetadata u) TTN.metadataFrequency) (fromMaybe (TTN.uplinkConfig u) TTN.configFrequency)
  where render m x = tr (lang m) (MsgFrequency $ adj x)
        adj x | x > 1000 = x / 1000000
        adj x | otherwise = x

renderDataRate m u = choose m render (fromMaybe (TTN.uplinkMetadata u) TTN.metadataDataRate) (fromMaybe (TTN.uplinkConfig u) TTN.configDataRate)
  where render m x = tr (lang m) (MsgDataRate x)

-- nanoseconds, seems broken
renderAirTime m u = choose m render (fromMaybe (TTN.uplinkMetadata u) TTN.metadataAirtime) (fromMaybe (TTN.uplinkConfig u) TTN.configAirtime)
  where render m x = text $ pack $ showFloat x

fromMaybe Nothing _ = Nothing
fromMaybe (Just m) accessor = accessor m

fromText = text . pack . T.unpack

renderInt x = text $ pack $ show x

newNav Model{..} =
    nav_ [ class_ ("navbar navbar-expand-lg fixed-top navbar-dark bg-dark " <> (pack $ classByStatus connected)) ] [
        a_ [ href_ "/", onPreventClick (ChangeURI goHome), class_ "navbar-brand"] [
            i_ [ class_"fa fa-database", title_ "Vohoo"] [ ]
        ]
      , div_ [class_ "mr-auto", id_ "navbarNav" ] [
          ul_ [class_ "navbar-nav flex-row"] [
            li_ [class_ "nav-item"] [
              a_ [href_ "/", onPreventClick (ChangeURI goHome), class_ "nav-link"] [ tr lang MsgHome ]
            ]
          , li_ [class_ "nav-item"] [
              a_ [href_ "/docs", onPreventClick (ChangeURI goDocs), class_ "nav-link"] [ tr lang MsgDocs ]
            ]
          , li_ [class_ "nav-item"] [
              a_ [href_ "/about", onPreventClick (ChangeURI goAbout), class_ "nav-link"] [ tr lang MsgAbout ]
            ]
          ]
        ]
      , (iff connected $ numClients lang clients)
      , (iff connected $ (div_ [ id_ "messageCount" ] [
          i_ [ class_ "fa fa-envelope" ] [ ]
          , span_ [ class_ "textual" ] [ tr lang (MsgMessageCount eventCount) ]
        ]))
      , (connectionStatus lang connected)
      , div_ [ id_ "settings" ] [
          a_ [ onPreventClick ToggleSettings ] [
            i_ [ class_ "fa fa-cog" ] [ ]
            , span_ [ class_ "textual" ] [ tr lang MsgSettings ]
          ]
        ]
      , div_ [ id_ "language" ] [
          a_ [ onPreventClick ToggleSettings ] [
            i_ [ class_ "fa fa-language" ] [ ]
            , span_ [ class_ "textual" ] [ tr lang MsgLanguage ]
          ]
        ]
    ]

the404 :: Model -> View Action
the404 = template v
  where
    v = div_ [] [
         h1_ [ class_  "title"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
         ] [ text "404" ]
       , h2_ [ class_  "subtitle" ] [
          text "No data for you! "
          , a_ [ href_ "/", onPreventClick (ChangeURI goHome) ] [ text " - Go Home" ]
         ]
       ]

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions defaultOptions { preventDefault = True }
    "click" emptyDecoder (\() -> action)

-- | Links
goHome, goAbout, goDocs, goCommunity :: URI
( goHome, goAbout, goDocs, goCommunity ) =
    ( linkURI (safeLink routes homeProxy)
    , linkURI (safeLink routes aboutProxy)
    , linkURI (safeLink routes docsProxy)
    , linkURI (safeLink routes communityProxy)
    )

homeProxy :: Proxy Home
homeProxy = Proxy
aboutProxy :: Proxy About
aboutProxy = Proxy
docsProxy :: Proxy Docs
docsProxy = Proxy
communityProxy :: Proxy Community
communityProxy = Proxy
routes :: Proxy ClientRoutes
routes = Proxy

-- plotting experiment, too boring with temp/humidity
plot events = div_ [ id_ "plot" ] [
    svg_ [ style_ $ M.fromList [ ("max-height", "256px")
                               , ("width", "100%")
                               ]
         , viewBox_ "0 0 200 100"
         ] [
        g_ [ class_ "grid x-grid" ] [
          line_ [ x1_ "5", x2_ "195", y1_ "95", y2_ "95", style_ axisStyle ] []
        ]
      , g_ [ class_ "grid y-grid" ] [
          line_ [ x1_ "5", x2_ "5", y1_ "5", y2_ "95", style_ axisStyle ] []
        ]
      , g_ [ class_ "points" ] (map (\(x, y) -> circle_ [ cx_ $ ms $ show x, cy_ $ ms $ show y, r_ "1", style_ pointStyle ] []) tempXY)
      , g_ [ class_ "line" ] [ polyline_ [ points_ $ ms (Prelude.concatMap (\(x, y) -> show x <> "," <> show y <> "\n") tempXY), style_ lineStyle ] [] ]

      , g_ [ class_ "points" ] (map (\(x, y) -> circle_ [ cx_ $ ms $ show x, cy_ $ ms $ show y, r_ "1", style_ pointStyle2 ] []) humiXY)
      , g_ [ class_ "line" ] [ polyline_ [ points_ $ ms (Prelude.concatMap (\(x, y) -> show x <> "," <> show y <> "\n") humiXY), style_ lineStyle2 ] [] ]
    ]
  ]
  where
    filtered = filter tempHumOnly events
    stamps = map (\(Evt _ srvtime _) -> zonedToTimestamp srvtime) filtered

    thValues = map extract $ filtered
    extract (Evt (TTN.Event typ u@TTN.Uplink{..}) srvtime decoded) = let th = head . filter isTH $ decoded in (getTemp th, getHumi th)

    temps = map fst thValues
    humis = map snd thValues

    minTS = minimum stamps
    maxTS = (maximum stamps) + 1 -- + 1200 -- extrapolate 1200seconds (10 messages, each takes 2 minutes)

    minTemp = minimum temps

    scaledStamps = map (scaleRange (minTS, maxTS) (6, 194)) stamps
    --scaledTemps = map (scaleRange (minTemp, 40) (5, 95)) temps
    --scaledHumis = map (scaleRange (20, 80) (5, 95)) humis

    inv = map (100-)

    tempXY = zip scaledStamps (inv temps)
    humiXY = zip scaledStamps (inv humis)

scaleRange :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleRange (fromLow, fromHigh) (toLow, toHigh) x = (x - fromLow) * (toHigh - toLow) / (fromHigh - fromLow) + toLow

axisStyle :: M.Map MisoString MisoString
axisStyle =
  M.fromList [
      ("stroke", "WhiteSmoke")
    , ("stroke-width", "1")
    , ("stroke-dasharray", "0")
    ]

lineStyle :: M.Map MisoString MisoString
lineStyle =
  M.fromList [
      ("stroke", "#FF4500")
    , ("stroke-width", "1")
    ]

pointStyle :: M.Map MisoString MisoString
pointStyle =
  M.fromList [
      ("fill", "#CC4500")
    ]

lineStyle2 :: M.Map MisoString MisoString
lineStyle2 =
  M.fromList [
      ("stroke", "#1E90FF")
    , ("stroke-width", "1")
    ]

pointStyle2 :: M.Map MisoString MisoString
pointStyle2 =
  M.fromList [
      ("fill", "#1E90CC")
    ]

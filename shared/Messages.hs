{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Messages where

import           Miso
import           Miso.String
import           Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

import Text.Shakespeare.I18N

-- TH i18n
-- http://happstack.com/docs/crashcourse/Templates.html#hsp-i18n

data MyApp = MyApp

mkMessage "MyApp" "messages" "en"

tr :: RenderMessage MyApp message => Lang -> message -> View action
tr lang thing = text $ pack $ T.unpack $ renderMessage MyApp [lang] thing

trTitle :: RenderMessage MyApp message => Lang -> message -> Attribute action
trTitle lang thing = title_ $ pack $ T.unpack $ renderMessage MyApp [lang] thing

showFloat :: (PrintfArg a, Real a) => a -> String
showFloat = printf "%.1f"

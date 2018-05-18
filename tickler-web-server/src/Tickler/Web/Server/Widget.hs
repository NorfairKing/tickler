module Tickler.Web.Server.Widget where

import Import

import Data.Default
import Language.Haskell.TH.Syntax (Exp, Q)

import Yesod.Default.Util
       (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

import Tickler.Web.Server.Constants

widgetFile :: String -> Q Exp
widgetFile =
    if development
        then widgetFileReload widgetFileSettings
        else widgetFileNoReload widgetFileSettings

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

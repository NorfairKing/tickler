module Tickler.Web.Server.Widget where

import Data.Default
import Import
import Language.Haskell.TH.Syntax (Exp, Q)
import Tickler.Web.Server.Constants
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFile :: String -> Q Exp
widgetFile =
  if development
    then widgetFileReload widgetFileSettings
    else widgetFileNoReload widgetFileSettings

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

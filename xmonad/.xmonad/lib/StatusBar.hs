{-# LANGUAGE FlexibleContexts #-}
module StatusBar (myStatusBar) where

import Data.Monoid (All(..))

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Layout.LayoutModifier

handleIfXmobar :: Window -> X All
handleIfXmobar w = do
  whenX (runQuery (appName =? "xmobar") w) $ do
    spawn "~/.xmonad/tray.sh"
  return (All True)

trayerHook :: Event -> X All
trayerHook (MapNotifyEvent { ev_window = w }) = handleIfXmobar w
trayerHook (ConfigureEvent { ev_window = w }) = handleIfXmobar w
trayerHook _  = return (All True)

pp :: PP -- pretty printer
pp = xmobarPP
  { ppTitle = xmobarColor "#FFB6B0" "" . shorten 40
  , ppCurrent = xmobarColor "#CEFFAC" "" . wrap "[" "]"
  , ppSep = "   "
  }

-- Status bar is composed of xmobar and trayer, arranged side-by-side.
-- Strategy is that we launch xmobar, then on the MapNotify event from
-- the xmobar window we'll launch trayer. The tray.sh script kills
-- existing trayer instances, finds where xmobar is located, and
-- positions itself accordingly. We'll also listen for ConfigureNotify
-- events to catch when xmobar switches monitors and respawn trayer in the correct place.
mySB :: StatusBarConfig
mySB = statusBarProp "xmobar" (pure pp)
myStatusBar :: LayoutClass l Window
          => XConfig l -- ^ the base config
          -> XConfig (ModifiedLayout AvoidStruts l)
myStatusBar conf =
  withEasySB mySB defToggleStrutsKey $
  conf {
  handleEventHook = mconcat
                    [ trayerHook
                    , handleEventHook conf]
  }

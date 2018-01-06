{-# LANGUAGE FlexibleContexts #-}
module StatusBar (myStatusBar) where

import Data.Monoid (All(..), mconcat)
import System.Posix.IO (closeFd, createPipe, dupTo, fdToHandle, setFdOption, stdInput, FdOption(..))
import System.Posix.Process (executeFile)
import System.IO (hPutStrLn, hSetBuffering, BufferMode(..))

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.ManageHook ((=?), appName)

handleIfXmobar :: Window -> X All
handleIfXmobar w = do
  whenX (runQuery (appName =? "xmobar") w) $ do
    spawn "~/.xmonad/tray.sh"
    docksStartupHook
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
--
-- This is mostly the same as the "statusBar" helper function from
-- xmonad-contrib. The difference is we create a pipe to use in the
-- logHook, but wait until the *startupHook* to kickoff xmobar, as
-- we're not guaranteed xmonad will be listening to events until after
-- the IO (XConfig...) from this function is computed. xmonad executes
-- the startupHook after it has registered to listen for window events
myStatusBar :: LayoutClass l Window
          => XConfig l -- ^ the base config
          -> IO (XConfig (ModifiedLayout AvoidStruts l))
myStatusBar conf = do
  (rd, wr) <- createPipe
  setFdOption wr CloseOnExec True
  h <- fdToHandle wr
  hSetBuffering h LineBuffering
  return $ docks $
    conf
    { layoutHook = avoidStruts (layoutHook conf)
    , manageHook = manageDocks <+> manageHook conf
    , logHook = do
        logHook conf
        dynamicLogWithPP pp { ppOutput =  hPutStrLn h }
    , startupHook = do
        startupHook conf
        _ <- xfork $ do
          _ <- dupTo rd stdInput
          executeFile "/bin/sh" False ["-c", "xmobar"] Nothing
        io $ closeFd rd
        return ()
    , handleEventHook = mconcat
      [ trayerHook
      , docksEventHook
      , handleEventHook conf]
    }

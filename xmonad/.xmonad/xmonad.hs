import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docksStartupHook)
import XMonad.Operations (rescreen)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.WindowProperties (getProp32)

-- XMonad configuration *without* xmobar-related items
conf = ewmh $ fullscreenSupport $ def
  { terminal = "urxvt -e ~/launch-tmux.sh"
  , modMask = mod4Mask
  , startupHook = mappend (startupHook def) setFullscreenSupported
  }
  `additionalKeysP`
  [ ("M-S-l", spawn "xscreensaver-command -lock")
  , ("M-S-/", spawn ("echo -e " ++ show help ++ " | xmessage -file -"))
  , ("M-x r", rescreen)
  , ("M-x d", docksStartupHook)
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  -- Turn output on if it's off, otherwise bump volume
  , ("<XF86AudioRaiseVolume>", spawn "amixer get Master | grep -q off && amixer set Master on || amixer set Master 5%+")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")]

-- xmobar config
main :: IO ()
main = xmonad =<< statusBar "xmobar"
  xmobarPP
  { ppTitle = xmobarColor "#FFB6B0" "" . shorten 40
  , ppCurrent = xmobarColor "#CEFFAC" "" . wrap "[" "]"
  , ppSep = "   "
  } (const (modMask conf, xK_b)) conf

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:"
    , ""
    , "-- launching and killing programs"
    , "mod-Shift-Enter  Launch xterminal"
    , "mod-p            Launch dmenu"
    , "mod-Shift-p      Launch gmrun"
    , "mod-Shift-c      Close/kill the focused window"
    , "mod-Space        Rotate through the available layout algorithms"
    , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
    , "mod-n            Resize/refresh viewed windows to the correct size"
    , ""
    , "-- move focus up or down the window stack"
    , "mod-Tab        Move focus to the next window"
    , "mod-Shift-Tab  Move focus to the previous window"
    , "mod-j          Move focus to the next window"
    , "mod-k          Move focus to the previous window"
    , "mod-m          Move focus to the master window"
    , ""
    , "-- modifying the window order"
    , "mod-Return   Swap the focused window and the master window"
    , "mod-Shift-j  Swap the focused window with the next window"
    , "mod-Shift-k  Swap the focused window with the previous window"
    , ""
    , "-- resizing the master/slave ratio"
    , "mod-h  Shrink the master area"
    , "mod-l  Expand the master area"
    , ""
    , "-- floating layer support"
    , "mod-t  Push window back into tiling; unfloat and re-tile it"
    , ""
    , "-- increase or decrease number of windows in the master area"
    , "mod-comma  (mod-,)   Increment the number of windows in the master area"
    , "mod-period (mod-.)   Deincrement the number of windows in the master area"
    , ""
    , "-- quit, or restart"
    , "mod-Shift-q  Quit xmonad"
    , "mod-q        Restart xmonad"
    , "mod-[1..9]   Switch to workSpace N"
    , ""
    , "-- Workspaces & screens"
    , "mod-Shift-[1..9]   Move client to workspace N"
    , "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3"
    , "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3"
    , ""
    , "-- Mouse bindings: default actions bound to mouse events"
    , "mod-button1  Set the window to floating mode and move by dragging"
    , "mod-button2  Raise the window to the top of the stack"
    , "mod-button3  Set the window to floating mode and resize by dragging"
    , ""
    , "-- Custom"
    , "mod-d         Dismiss least-recent dunst notification"
    , "mod-Shift-d   Dismiss *all* dunst notifications"
    , "mod-grave     Show notification from dunst history"
    , "mod-Shift-.   Show dunst notification context menu"
    , ""
    , "mod-Shift-l   Lock screen with xscreensaver"]

-- Add _NET_WM_STATE_FULLSCREEN to the _NET_SUPPORTED property of the
-- root window to advertise support for it (if not already present).
-- The XMonad.Hooks.EwmhDesktops.fullscreenEventHook function is used
-- to back this. Core XMonad does not support this yet, and EWMH
-- doesn't seem to already have a helper to announce support:
-- https://www.reddit.com/r/xmonad/comments/77szad/cant_go_fullscreen_in_firefox_even_with_ewmh/doof76r/
setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    netSupported <- getAtom "_NET_SUPPORTED"
    tAtom <- getAtom "ATOM"
    stateFullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
    curSupported <- getProp32 netSupported r
    case curSupported of
      Just props -> if sf `elem` props
                    then return ()
                    else io $ changeProperty32 dpy r netSupported tAtom propModeAppend [sf]
                    where
                      sf = fromIntegral stateFullscreen
      Nothing -> return ()

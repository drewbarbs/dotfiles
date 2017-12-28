import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.WindowProperties (getProp32)

conf = ewmh def
  { terminal = "urxvt -e ~/launch-tmux.sh"
  , modMask = mod4Mask
  , handleEventHook = mappend (handleEventHook def) fullscreenEventHook
  , startupHook = mappend (startupHook def) setFullscreenSupported
  }
  `additionalKeysP`
  [ ("M-S-l", spawn "xscreensaver-command -lock")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioMute>", spawn "amixer set Master 0%")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")]

main = xmonad =<< xmobar conf

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

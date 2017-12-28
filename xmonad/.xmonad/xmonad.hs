import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig (additionalKeysP)

conf = ewmh def
  { terminal = "urxvt -e ~/launch-tmux.sh"
  , modMask = mod4Mask
  , handleEventHook = mappend (handleEventHook def) fullscreenEventHook
  }
  `additionalKeysP`
  [ ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioMute>", spawn "amixer set Master 0%")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")]

main = xmonad =<< xmobar conf

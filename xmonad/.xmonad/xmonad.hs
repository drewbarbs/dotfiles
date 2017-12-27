import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig (additionalKeysP)

conf = def
  { terminal = "urxvt -e ~/launch-tmux.sh"
  , modMask = mod4Mask
  }
  `additionalKeysP`
  [ ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioMute>", spawn "amixer set Master 0%")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")]

main = xmonad =<< xmobar conf

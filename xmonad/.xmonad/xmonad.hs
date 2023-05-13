import XMonad

import Data.Monoid (All(..), mconcat)
import Graphics.X11.Xrandr (xrrSelectInput)
import System.Exit (ExitCode(..), exitWith)
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.Place (inBounds, placeHook, underMouse)
import XMonad.Layout.MultiToggle (Toggle(..), mkToggle, single)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Reflect (REFLECTX(..))
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.Spacing (Border(..), SpacingModifier(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Operations (rescreen)
import XMonad.Prompt (XPConfig(..))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.WindowProperties (getProp32)

import StatusBar

myLayoutHook =
  (mkToggle (single REFLECTX) tall) |||
  Mirror tall |||
  ThreeCol 1 (3 / 100) (1 / 2) |||
  (renamed [Append "Mid"] $ ThreeColMid 1 (3 / 100) (1 / 2)) ||| Full
  where
    tall =
      renamed [CutWordsLeft 1] -- Cut the "Spaced" from the layout name
            -- Start with gaps disabled, Can press M-g to toggle them
       $
      spacingRaw True (Border 0 5 5 5) False (Border 5 5 5 5) False $
      Tall 1 (3 / 100) (1 / 2)

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ appName =? "gnome-calendar" --> floatRelative 0.5
    , appName =? "zenity" --> floatRelative 0.5
    , appName =? "gnome-system-monitor" --> floatRelative 0.9
    , className =? "Org.gnome.Weather" --> floatRelative 1
    ]
    -- float the window, place it under the mouse where x controls how
    -- far across the top of the window (0 = left corner, 1 = right
    -- corner) the mouse pointer should be located
  where
    floatRelative x = (placeHook $ inBounds (underMouse (x, 0.1))) <+> doFloat

-- XMonad configuration *without* xmobar-related items
conf =
  EWMH.ewmhFullscreen . EWMH.ewmh $
  def
      --terminal = "urxvtc -e ~/launch-tmux.sh"
    { terminal = "gnome-terminal"
    , layoutHook = smartBorders myLayoutHook
    , manageHook = myManageHook <+> (manageHook def)
    , modMask = mod4Mask
    , startupHook =
        mconcat [startupHook def, setFullscreenSupported, registerForXRREvents]
    , clientMask = (clientMask def) .|. rrScreenChangeNotifyMask
    , handleEventHook =
        mconcat [handleEventHook def, xrrHook]
    } `additionalKeysP`
  [ ( "M-p"
    , spawn "rofi -modi combi,window,ssh -show combi -combi-modi drun,run")
  , ("M-S-p", spawn "dmenu_run")
  , ("M-S-m", spawn "emacsclient -c")
  , ("M-S-l", spawn "xscreensaver-command -lock")
  , ("M-S-/", spawn ("echo -e " ++ show help ++ " | xmessage -file -"))
  , ( "M-g"
    , mconcat
        [ sendMessage $ ModifyScreenBorderEnabled not
        , sendMessage $ ModifyWindowBorderEnabled not
        ])
  , ("M-S-b", spawn "killall -s SIGUSR1 xmobar")
  , ( "M-S-q"
    , confirmPrompt (def {height = 60}) "exit" $ io (exitWith ExitSuccess))
  , ("M-x r", rescreen)
  , ("M-x f", sendMessage $ Toggle REFLECTX)
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  -- Turn output on if it's off, otherwise bump volume
  , ( "<XF86AudioRaiseVolume>"
    , spawn
        "amixer get Master | grep -q off && amixer set Master on || amixer set Master 5%+")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  ]

main :: IO ()
main = xmonad $ myStatusBar conf

help :: String
help =
  unlines
    [ "Keybindings:"
    , ""
    , "-- launching and killing programs"
    , "mod-Shift-Enter  Launch terminal window"
    , "mod-Shift-e      Launch emacsclient window"
    , "mod-p            Launch rofi"
    , "mod-Shift-p      Launch dmenu"
    , "mod-Shift-c      Close/kill the focused window"
    , ""
    , "-- Layout"
    , "mod-Space        Rotate through the available layout algorithms"
    , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
    , "mod-n            Resize/refresh viewed windows to the correct size"
    , "mod-b            Toggle Struts (status bar)"
    , "mod-g            Toggle gaps"
    , "mod-x f          Apply REFLECTX modifier (tall layout only)"
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
    , "-- resizing the primary/secondary ratio"
    , "mod-h  Shrink the primary area"
    , "mod-l  Expand the primary area"
    , ""
    , "-- floating layer support"
    , "mod-t  Push window back into tiling; unfloat and re-tile it"
    , ""
    , "-- increase or decrease number of windows in the primary area"
    , "mod-comma  (mod-,)   Increment the number of windows in the primary area"
    , "mod-period (mod-.)   Deincrement the number of windows in the primary area"
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
    , "-- Notifications"
    , "mod-d         Dismiss least-recent dunst notification"
    , "mod-Shift-d   Dismiss *all* dunst notifications"
    , "mod-grave     Show notification from dunst history"
    , "mod-Shift-.   Show dunst notification context menu"
    , ""
    , "-- Misc"
    , "mod-Shift-b   Cycle statusbar to the next monitor"
    , "mod-Shift-l   Lock screen with xscreensaver"
    , ""
    , "-- Debug"
    , "mod-x r       xmonad \"rescreen\""
    ]

-- Add _NET_WM_STATE_FULLSCREEN to the _NET_SUPPORTED property of the
-- root window to advertise support for it (if not already present).
-- The XMonad.Hooks.EwmhDesktops.fullscreenEventHook function is used
-- to back this. Core XMonad does not support this yet, and EWMH
-- doesn't seem to already have a helper to announce support:
-- https://www.reddit.com/r/xmonad/comments/77szad/cant_go_fullscreen_in_firefox_even_with_ewmh/doof76r/
setFullscreenSupported :: X ()
setFullscreenSupported =
  withDisplay $ \dpy -> do
    r <- asks theRoot
    netSupported <- getAtom "_NET_SUPPORTED"
    tAtom <- getAtom "ATOM"
    stateFullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
    curSupported <- getProp32 netSupported r
    case curSupported of
      Just props ->
        if sf `elem` props
          then return ()
          else io $
               changeProperty32 dpy r netSupported tAtom propModeAppend [sf]
        where sf = fromIntegral stateFullscreen
      Nothing -> return ()

registerForXRREvents :: X ()
registerForXRREvents =
  withDisplay $ \dpy ->
    asks theRoot >>= \r -> io $ xrrSelectInput dpy r rrScreenChangeNotifyMask

resetBackground :: X All
resetBackground = do
  spawn "feh --bg-fill $(variety --current | tail -n1)"
  return (All True)

xrrHook :: Event -> X All
xrrHook (RRScreenChangeNotifyEvent {}) = resetBackground
xrrHook _ = return (All True)

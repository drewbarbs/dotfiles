module Main where

import Control.Exception (bracket)
import Control.Monad (filterM)
import Control.Monad.Except (catchError)
import Foreign.C.String (peekCString)
import System.Exit (exitFailure)
import Text.Printf (printf)

import qualified Graphics.UI.Gtk as G
import Graphics.X11.Types
import Graphics.X11.Xlib.Types
import Graphics.X11 (defaultRootWindow, openDisplay, closeDisplay)
import Graphics.X11.Xlib.Atom (wM_CLASS)
import Graphics.X11.Xlib.Extras (getTextProperty, getWindowAttributes, queryTree,
                                 tp_value, wa_map_state, waIsViewable)
import Graphics.X11.Xlib.Misc (getGeometry)

isxmobar :: Display -> Window -> IO Bool
isxmobar dpy win = catchError checkWMClass (const (return False))
  where checkWMClass = do
          p <- getTextProperty dpy win wM_CLASS
          pStr <- peekCString (tp_value p)
          attribs <- getWindowAttributes dpy win
          return $ pStr == "xmobar" && (wa_map_state attribs) == waIsViewable

xmobarGeom :: Display -> IO (Maybe Rectangle)
xmobarGeom dpy = do
  (_, _, wins) <- queryTree dpy (defaultRootWindow dpy)
  matches <- filterM (isxmobar dpy) wins
  case matches of
    win:_ -> do
      (_, x, y, w, h, _, _) <- getGeometry dpy win
      return $ Just (Rectangle x y w h)
    _ -> return Nothing

withGdkDisplay :: (G.Display -> IO a) -> IO a
withGdkDisplay f = bracket (G.displayGetDefault)
    (\disp -> case disp of
                Just dpy -> G.displayClose dpy
                _ -> return ())
    (\disp -> do
        let (Just dpy) = disp
        f dpy)

main :: IO ()
main = bracket (openDisplay []) (closeDisplay)
       (\dpy -> do
           _ <- G.initGUI
           geom <- xmobarGeom dpy
           case geom of
             Just rect -> do
               let (Rectangle xmbX xmbY xmbW xmbH) = rect
               withGdkDisplay (\gDpy -> do
                                  scr <- G.displayGetDefaultScreen gDpy
                                  mon <- G.screenGetMonitorAtPoint scr (fromIntegral xmbX) (fromIntegral xmbY)
                                  G.Rectangle _ _ monW _ <- G.screenGetMonitorGeometry scr mon
                                  printf "%d %d %d" mon ((fromIntegral monW) - xmbW) xmbH)
             _ -> exitFailure)

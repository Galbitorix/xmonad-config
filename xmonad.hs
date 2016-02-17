import           XMonad
import qualified XMonad.StackSet           as W
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout
import           XMonad.Operations
import           XMonad.ManageHook
import           Data.Monoid
import           System.Exit
import qualified Data.Map                  as M
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.FadeWindows
import           XMonad.Layout.Fullscreen
-- import qualified System.Dzen.Padding       as D
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.Spacing
import           XMonad.Layout.NoBorders
import qualified XMonad.Layout.HintedTile as T
import           XMonad.Layout.ResizableTile
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.FadeInactive
-- to get conky, use 2 dzen bars

main :: IO()
main = do dzproc <- spawnPipe $ myStatusBar
          dzstatus <- spawnPipe $ myConkyBar
          -- dock <- spawnPipe "GTK2_RC_FILES=/home/frank/.xmonad/gtkdocky /usr/bin/docky"
          xmonad $ additionalKeysP (myConfig dzproc) addedKeys

myConfig dzproc = defaultConfig {
            normalBorderColor = color8
            , focusedBorderColor = background -- color4
            , terminal = myTerm
            , layoutHook = myLayoutHook
            , manageHook = manageHook defaultConfig <+> manageDocks <+> myManageHook
            , handleEventHook = fadeWindowsEventHook <+> fullscreenEventHook <+> docksEventHook
            , workspaces = myWorkspaces
            , modMask = myModMask -- seach -> chromebook, windows -> desktop
            -- , keys = keyBinds
            -- , mouseBindings = myMouseBindings
            , borderWidth = 1
            , logHook = myLogHook dzproc
            , startupHook = setWMName "LG3D" -- myStartupHook
            , focusFollowsMouse = True
            , clickJustFocuses = True
            } -- record syntax of data type defaultConfig


--working on it
myLayoutHook = smartBorders $ avoidStruts $
               verticalTile               |||
               horizTile                  |||
               Full
               where
                 verticalTile = smartSpacing 5 $ Tall 1 (3/100) (1/2)
                 horizTile = Mirror $ smartSpacing 5 $ Tall 1 (3/100) (1/2)

addedKeys :: [(String, X ())]
addedKeys = [ ("M-<Return>"  , spawn myTerm),
              ("M-e"         , spawn "emacs"),
              ("M-i"         , spawn "chromium"),
              ("M-S-t"       , spawn "thunar"),
              ("M-<Left>"    , windows W.swapMaster >> windows W.focusDown),
              ("M-<F7>"      , spawn "xbacklight -inc 10"),
              ("M-<F6>"      , spawn "xbacklight -dec 10"),
              ("M-n"         , spawn "netbeans"),
              ("M-r"         , spawn "evince"),
              ("M-m"         , spawn "nylas")
            ]

myManageHook = composeAll [ resource =? "xterm" --> doFloat
                          , resource =? "emacs" --> doShift (myWorkspaces !! 2)
                          , className =? "chromium" --> doShift (myWorkspaces !! 1)
                          , className =? "NetBeans IDE 8.0.2" --> doShift (myWorkspaces !! 3)
                          , className =? "Nylas N1" --> doShift (myWorkspaces !! 5)
                          ] -- get className with xprop

myFadeLogHook = composeAll [isUnfocused --> transparency 0.8
                           , opaque
                           ]

myLogHook dzproc = dynamicLogWithPP $ dzenPP {
      ppOutput = hPutStrLn dzproc
    , ppTitle =  wrap "^ca(1,/home/frank/.xmonad/scripts/popterm.sh)" "^ca()" . pad  . shorten 50
    , ppLayout = dzenColor color4 background . (\x -> case x of
                                             "SmartSpacing 5 Tall" -> "    ^i(/home/frank/.xmonad/icons/tiling.xbm)   "
                                             "Mirror SmartSpacing 5 Tall" -> "    ^i(/home/frank/.xmonad/icons/mirrortall.xbm)   "
                                             "Full" -> "    ^i(/home/frank/.xmonad/icons/floating.xbm)   "
                                             _ -> "   New Layout   "
                                             )

    , ppCurrent = dzenColor foreground background -- foreground "#FF6000"
    , ppVisible = dzenColor color4 background
    , ppHidden = dzenColor color4 background  -- foreground "#7BB352"
    , ppHiddenNoWindows = dzenColor color8 background
    , ppOrder = \(ws:l:t:_) -> [ws,l,t]
    }

myStartupHook :: X ()
myStartupHook = return ()

myModMask = mod4Mask

myTerm :: String
myTerm = "urxvt"

myWorkspaces :: [String]
-- myWorkspaces = ["  ê  ", " web ", " code "," devel "," media "," misc "]
-- myWorkspaces = [icon ++ "\x00e", "\x00e02d", "\x00e1ce", "\x00e223", "\x00e09f", "\x00e14d"]
myWorkspaces = [music, web, code, devel, media, mail, misc]
               where music = "   ^i(/home/frank/.xmonad/icons/Music.xbm)   "
                     web   = "   ^i(/home/frank/.xmonad/icons/www.xbm)   "
                     code  = "   ^i(/home/frank/.xmonad/icons/code.xbm)   "
                     devel = "   ^i(/home/frank/.xmonad/icons/Devel.xbm)   "
                     media = "   ^i(/home/frank/.xmonad/icons/media.xbm)   "
                     mail  = "   ^i(/home/frank/.xmonad/icons/mail.xbm)   "
                     misc  = "   ^i(/home/frank/.xmonad/icons/pacman.xbm)   "

myStatusBar, myConkyBar, myFont :: String
myStatusBar = "dzen2 -x 0 -w '683' -h '28' -ta l -xs 1 -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn '" ++ myFont ++ "'"
myConkyBar = "conky -c ~/.xmonad/conky_dzen | dzen2 -ta r -x '683' -w '683' -h '28' -p $OPTS -xs 1 -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn '" ++ myFont ++ "'"

--myFont =  "-*-ohsnap.icons-medium-r-*-*-14-*-*-*-*-*-*-*"
-- myFont = "-*-terminus-*-r-*-*-12-*-*-*-*-*-*-*"
-- myFont = "-*-nu-*-*-*-*-14-*-*-*-*-*-*-*"
myFont = "-*-ubuntu-*-*-*-*-12-*-*-*-*-*-*-*"
-- myFont = "-wuncon-siji-medium-r-normal--17-100-75-75-c-80-iso10646-1"

--CLOUDS
background= "#232323" -- "#0E0E0E"
foreground= "#CBCBCB"
color0= "#454545"
color8= "#676767"
color1=  "#CC4747"
color9=  "#BF5858"
color2=  "#A0CF5D"
color10= "#B8D68C"
color3=  "#FF9B52"
color11= "#FFB680"
color4=  "#307D92" -- "#508934" -- "#AB2010" -- "#99492F"
color12= "#99C7BF"
color5=  "#A966CC"
color13= "#BD9FCC"
color6=  "#6CAB79"
color14= "#95BA9C"
color7=  "#d3d3d3"
color15= "#fefefe"

--color0 = "#2F2E2D"
--color8 = "#4A4845"
--color1 = "#A36666"
--color9 = "#D78787"
--color2 = "#8FA57E"
--color10 = "#A9BA9C"
--color3 = "#D7AF87"
--color11 = "#E4C9AF"
--color4 = "#7FA5BD"
--color12 = "#A1BDCE"
--color5 = "#C79EC4"
--color13 = "#D7BEDA"
--color6 = "#8ADBB4"
--color14 = "#B1E7DD"
--color7 = "#D0D0D0"
--color15 = "#EFEFEF"

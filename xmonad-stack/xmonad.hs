-------------------------------------------------------------------------------
-- Author: vijaysenpathi@gmail.com <Vijay Senapathi>
-------------------------------------------------------------------------------

import XMonad (
    xmonad, def, modMask, mod4Mask, terminal, (|||), (=?), (-->), (<+>),
    workspaces, composeAll,
    normalBorderColor, focusedBorderColor, borderWidth, focusFollowsMouse,
    startupHook, layoutHook, manageHook,
    className, doFloat,
    sendMessage, spawn
  )
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.ClickableWorkspaces (clickablePP)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar (withEasySB, statusBarProp, defToggleStrutsKey)
import XMonad.Hooks.StatusBar.PP (
    ppSep, ppWsSep, ppHidden, ppHiddenNoWindows, ppOrder, ppCurrent, ppVisible,
    PP, wrap, xmobarColor, ppExtras
  )
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.Place (placeHook, withGaps, smart)

import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout (Resize(..), Tall(..), Full(..), Mirror(..))
import XMonad.Layout.NoBorders (noBorders)

-- Window Navigation
import XMonad.Actions.Navigation2D (
    defaultTiledNavigation, floatNavigation, layoutNavigation,
    hybridOf, navigation2D,
    lineNavigation, centerNavigation, sideNavigationWithBias, sideNavigation,
    withNavigation2DConfig, windowGo,
    Direction2D(..)
  )

-------------------------------------------------------------------------------
-- Modifier key
-------------------------------------------------------------------------------
myModMask = mod4Mask
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Terminal
-------------------------------------------------------------------------------
myTerminal = "WINIT_X11_SCALE_FACTOR=1 alacritty"
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Workspaces
-------------------------------------------------------------------------------
myWorkspaces = 
 ["Home", "Web", "Term", "Code"] ++ map show [5..6]
 ++ ["Streams", "Music", "Chat"]
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Window setttings
-------------------------------------------------------------------------------
myNormalBorderColor  = "#6272A4"
myFocusedBorderColor = "#FF5555"
myBorderWidth = 5
myFocusFollowsMouse = False
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------------------
myKeyBindings = [
      ("M-p", spawn "rofi -show drun")

    -- Window Navigation
    , ("M-h", windowGo L False)
    , ("M-l", windowGo R False)
    , ("M-j", windowGo D False)
    , ("M-k", windowGo U False)

    -- Window manipulation
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-l", sendMessage Expand)
  ]
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------
layoutsHook = tiled ||| noBorders Full ||| Mirror tiled
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myLayoutHook = spacingRaw
  True
  (Border 0 10 10 10)
  True
  (Border 10 10 10 10)
  True
  $ layoutsHook
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- XMobar Properties
-------------------------------------------------------------------------------
myXMobarPP :: PP
myXMobarPP = def
  { ppSep             = " " -- Seperator between different log sections
  , ppWsSep           = " "
  , ppCurrent         = red . wrap "[" "]"
  , ppVisible         = yellow . wrap "[" "]"
  , ppHidden          = orange . wrap " " " "
  , ppHiddenNoWindows = wrap " " " "
  , ppOrder           = \(ws:_:t:xs) -> [ws]
  , ppExtras          = []
  }
  where
    orange  = xmobarColor "#FFB86C" ""
    yellow  = xmobarColor "#F1FA8C" ""
    red     = xmobarColor "#FF5555" ""
    cyan    = xmobarColor "#8BE9FD" ""
    green   = xmobarColor "#50FA7B" ""


myXMobarProp = withEasySB 
  (statusBarProp "xmobar -x 1 ~/.config/xmobar/xmobarrc" (clickablePP myXMobarPP))
  defToggleStrutsKey
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Startup
-------------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "feh --bg-scale ~/Pictures/Wallpapers/hooded_dark.jpg"
  spawnOnce "picom --config ~/.config/picom/picom.conf"
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  Window Management
-------------------------------------------------------------------------------
myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5))
myManageHook = placeHook myPlacement
  <+> composeAll
  [
    className =? "Xmessage" --> doFloat
  , manageDocks
  ]
  <+> manageHook def
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------
xmonadConfig = def
  {
    modMask               = myModMask
  , terminal              = myTerminal
  , layoutHook            = myLayoutHook
  , startupHook           = myStartupHook
  , manageHook            = myManageHook
  , workspaces            = myWorkspaces
  , normalBorderColor     = myNormalBorderColor
  , focusedBorderColor    = myFocusedBorderColor
  , borderWidth           = myBorderWidth
  , focusFollowsMouse     = myFocusFollowsMouse
  } 
  `additionalKeysP`
  myKeyBindings
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Window flags
-------------------------------------------------------------------------------
ewmhFlags = ewmhFullscreen . ewmh
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Navigation Config
-------------------------------------------------------------------------------
myNavigationConfig = def {
    defaultTiledNavigation  = sideNavigation
  , floatNavigation         = centerNavigation
}

myNavigator = withNavigation2DConfig myNavigationConfig
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
main :: IO ()
main = xmonad . ewmhFlags . myXMobarProp . myNavigator $ xmonadConfig
-------------------------------------------------------------------------------

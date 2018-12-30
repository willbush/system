import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    -- , layoutHook = avoidStruts  $ layoutHook defaultConfig
    , terminal = "alacritty"
    , modMask = mod4Mask -- Rebind Mod to the super key
    , normalBorderColor  = "#444444"
    , focusedBorderColor = "#999999"
    , borderWidth        = 1
    }

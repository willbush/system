import XMonad
import XMonad.Hooks.ManageDocks (manageDocks)
import qualified XMonad.Util.CustomKeys as C
import qualified XMonad.StackSet as W

main :: IO ()
main =
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , terminal = "alacritty"
    , modMask = mod4Mask -- Rebind Mod to the super key
    , keys = C.customKeys delkeys addkeys
    , normalBorderColor  = "#444444"
    , focusedBorderColor = "#999999"
    , borderWidth        = 1
    }
  where
    -- | Unbind keys I don't use.
    delkeys :: XConfig l -> [(KeyMask, KeySym)]
    delkeys XConfig {modMask = modm} =
      [ (modm,               xK_w)
      , (modm,               xK_e)
      , (modm,               xK_r)
      , (modm,               xK_p)
      , (modm .|. shiftMask, xK_w)
      , (modm .|. shiftMask, xK_e)
      , (modm .|. shiftMask, xK_r)
      , (modm .|. shiftMask, xK_p)
      ]
    -- | Rebind h, j, k, l keys to Colemak-DH keys m, n, e, i in the same
    -- positions.
    addkeys :: XConfig l -> [((KeyMask, KeySym), X ())]
    addkeys conf@(XConfig {modMask = modm}) =
      [ ((modm,               xK_n), windows W.focusDown)
      , ((modm,               xK_e), windows W.focusUp)
      , ((modm,               xK_m), sendMessage Shrink)
      , ((modm,               xK_i), sendMessage Expand)
      , ((modm .|. shiftMask, xK_n), windows W.swapDown)
      , ((modm .|. shiftMask, xK_e), windows W.swapUp)
      , ((modm .|. shiftMask, xK_m), windows W.focusMaster)
      ]

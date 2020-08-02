import           Data.Default                   ( def )
import           XMonad
import           XMonad.Hooks.ManageDocks       ( manageDocks )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import qualified System.Exit                   as X
import qualified XMonad.StackSet               as W
import qualified XMonad.Util.CustomKeys        as C

main :: IO ()
main =
  xmonad $ def
    { manageHook = manageDocks <+> manageHook def
    , startupHook = do
        setWMName "LG3D" -- Needed for Java GUI to work
        spawnOnce "feh --bg-scale /home/will/sync/images/retro.jpg"
        spawnOnce "albert &"
    , terminal = "alacritty"
    , modMask = mod4Mask -- Rebind Mod to the super key
    , keys = C.customKeys delkeys addkeys
    , normalBorderColor = "#444444"
    , focusedBorderColor = "#999999"
    , borderWidth = 1
    -- add left hand Colemak-DHm home keys and keys above the home keys as
    -- workspaces. This is very useful for 48 key keyboards like the Planck
    -- which has number keys accessible via a layer (modifier).
    , workspaces =
        [ "q", "w", "f", "p", "b",
          "a", "r", "s", "t", "g"
        ]
    }
  where
    delkeys :: XConfig l -> [(KeyMask, KeySym)]
    delkeys XConfig {modMask = modm} =
      -- Keys I'm going to rebind:
      [ (modm,               xK_h)
      , (modm,               xK_j)
      , (modm,               xK_k)
      , (modm,               xK_l)
      , (modm,               xK_m)
      , (modm,               xK_n)
      , (modm,               xK_q)
      , (modm,               xK_t)
      , (modm .|. shiftMask, xK_h)
      , (modm .|. shiftMask, xK_j)
      , (modm .|. shiftMask, xK_k)
      , (modm .|. shiftMask, xK_l)
      , (modm .|. shiftMask, xK_m)
      , (modm .|. shiftMask, xK_n)
      , (modm .|. shiftMask, xK_q)
      , (modm .|. shiftMask, xK_t)

      -- Unbind keys I don't use:
      , (modm,               xK_w)
      , (modm,               xK_e)
      , (modm,               xK_r)
      , (modm,               xK_p)
      , (modm .|. shiftMask, xK_w)
      , (modm .|. shiftMask, xK_e)
      , (modm .|. shiftMask, xK_r)
      , (modm .|. shiftMask, xK_p)
      ]

    workspaceKeys :: [KeySym]
    workspaceKeys = [ xK_q, xK_w, xK_f, xK_p, xK_b,
                      xK_a, xK_r, xK_s, xK_t, xK_g
                    ]

    addkeys :: XConfig l -> [((KeyMask, KeySym), X ())]
    addkeys conf@XConfig {modMask = modm} =
      -- Rebind h, j, k, l keys to Colemak-DHm keys m, n, e, i in the same
      -- positions.
      [ ((modm,               xK_n), windows W.focusDown)
      , ((modm,               xK_e), windows W.focusUp)
      , ((modm,               xK_m), sendMessage Shrink)
      , ((modm,               xK_i), sendMessage Expand)
      , ((modm .|. shiftMask, xK_n), windows W.swapDown)
      , ((modm .|. shiftMask, xK_e), windows W.swapUp)
      , ((modm .|. shiftMask, xK_m), windows W.focusMaster)
      -- quit, or restart
      , ((modm              , xK_x), spawn "xmonad --recompile && xmonad --restart")
      , ((modm .|. controlMask, xK_x), io X.exitSuccess)
      -- My mnemonic for 'd' is drop floating window
      , ((modm,               xK_d), withFocused $ windows . W.sink)
      -- Resize viewed windows to the correct size. I've never actually needed
      -- this. I'm not sure in what situation this would be useful.
      , ((modm,               xK_z), refresh)
      ] ++
      -- This is using a list comprehension to build a list of workspace key
      -- bindings.
      [ ((modm .|. m, k), windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys,
            (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

import           Data.Default                   ( def )
import           XMonad
import           XMonad.Hooks.ManageDocks       ( manageDocks )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import qualified System.Exit                   as X
import qualified XMonad.Layout.Grid            as L
import qualified XMonad.Layout.MultiToggle     as L
import qualified XMonad.Layout.Reflect         as L
import qualified XMonad.Layout.Spacing         as L
import qualified XMonad.Layout.ThreeColumns    as L
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
    , layoutHook = L.mkToggle (L.single L.REFLECTX) $  L.mkToggle (L.single L.REFLECTY) layouts
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

layouts = tallLeft ||| threeCol ||| Mirror tall ||| grid ||| Full
 where
  spaceBy :: Integer -> l a -> ModifiedLayout L.Spacing l a
  spaceBy n = L.spacingRaw True (L.Border n n n n) True (L.Border n n n n) True

  -- The default number of windows in the master pane
  nmaster  = 1
  -- Default proportion of screen occupied by master pane
  ratio    = 2 / 3
  -- Percent of screen to increment by when resizing panes
  delta    = 3 / 100
  tall     = spaceBy 5 $ Tall nmaster delta ratio
  -- tall layout with master on the right and stack on the left.
  tallLeft = spaceBy 5 $ L.reflectHoriz $ Tall nmaster delta ratio
  grid     = spaceBy 5 L.Grid
  threeCol = spaceBy 5 $ L.ThreeColMid 1 delta (1 / 2)

delkeys :: XConfig l -> [(KeyMask, KeySym)]
delkeys XConfig { modMask = modm } =
  [ (modm              , xK_h)
  , (modm              , xK_e)
  , (modm              , xK_j)
  , (modm              , xK_k)
  , (modm              , xK_l)
  , (modm              , xK_m)
  , (modm              , xK_n)
  , (modm              , xK_p)
  , (modm              , xK_q)
  , (modm              , xK_r)
  , (modm              , xK_t)
  , (modm              , xK_w)
  , (modm .|. shiftMask, xK_e)
  , (modm .|. shiftMask, xK_h)
  , (modm .|. shiftMask, xK_j)
  , (modm .|. shiftMask, xK_k)
  , (modm .|. shiftMask, xK_l)
  , (modm .|. shiftMask, xK_m)
  , (modm .|. shiftMask, xK_n)
  , (modm .|. shiftMask, xK_p)
  , (modm .|. shiftMask, xK_q)
  , (modm .|. shiftMask, xK_r)
  , (modm .|. shiftMask, xK_t)
  , (modm .|. shiftMask, xK_w)
  ]

workspaceKeys :: [KeySym]
workspaceKeys =
  [ xK_q, xK_w, xK_f, xK_p, xK_b,
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
  -- restart or kill
  , ((modm,                 xK_k), spawn "xmonad --recompile && xmonad --restart")
  , ((modm .|. controlMask, xK_k), io X.exitSuccess)
  -- My mnemonic for 'd' is drop floating window
  , ((modm, xK_d), withFocused $ windows . W.sink)
  -- Resize viewed windows to the correct size. I've never actually needed
  -- this. I'm not sure in what situation this would be useful.
  , ((modm, xK_z), refresh)
  -- Reflect layout across the x or y axis.
  , ((modm, xK_x), sendMessage $ L.Toggle L.REFLECTX)
  , ((modm, xK_y), sendMessage $ L.Toggle L.REFLECTY)
  ] ++
  -- This is using a list comprehension to build a list of workspace key
  -- bindings.
  [ ((modm .|. m, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys,
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

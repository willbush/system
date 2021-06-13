{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           XMonad
import           XMonad.Hooks.ManageDocks       ( manageDocks )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.ThreeColumns     ( ThreeCol(ThreeColMid) )
import           XMonad.Util.Types              ( Direction2D(U, D, L, R) )
import qualified System.Exit                   as X
import qualified XMonad.Layout.Decoration      as D
import qualified XMonad.Layout.Grid            as L
import qualified XMonad.Layout.MultiToggle     as MT
import qualified XMonad.Layout.Reflect         as L
import qualified XMonad.Layout.Spacing         as L
import qualified XMonad.StackSet               as W
import qualified XMonad.Util.CustomKeys        as C

main :: IO ()
main =
  xmonad $ def
    { manageHook = manageDocks <+> manageHook def
    , startupHook = do
        setWMName "LG3D" -- Needed for Java GUI to work
        spawn "feh --bg-scale /home/will/sync/images/typeII.png"
    , layoutHook = MT.mkToggle (MT.single L.REFLECTX) layouts
    , terminal = "alacritty"
    , modMask = mod4Mask -- Rebind Mod to the super key
    , keys = C.customKeys delkeys addkeys
    , borderWidth = 0
    -- add left hand Colemak-DHm home keys and keys above the home keys as
    -- workspaces. This is very useful for 48 key keyboards like the Planck
    -- which has number keys accessible via a layer (modifier).
    , workspaces =
        [ "q", "w", "f", "p", "b",
          "a", "r", "s", "t", "g"
        ]
    }

layouts = tallLeft ||| threeCol ||| tallMirror ||| grid ||| full
 where
  tall       = Tall nmaster delta ratio
  -- tall layout with master on the right and stack on the left.
  tallLeft   = addTopBar $ spaceBy5 $ L.reflectHoriz tall
  tallMirror = addTopBar $ spaceBy5 $ Mirror tall
  grid       = addTopBar $ spaceBy5 L.Grid
  threeCol   = addTopBar $ spaceBy5 $ ThreeColMid 1 delta (1 / 2)
  full       = Full

  -- Add space between windows when more than one window.
  spaceBy5 :: l a -> ModifiedLayout L.Spacing l a
  spaceBy5 = L.spacingRaw True (L.Border n n n n) True (L.Border n n n n) True
    where n = 5

  addTopBar = smartBarDeco
    U
    def { D.activeBorderColor   = active
        , D.activeColor         = active
        , D.activeTextColor     = active
        , D.decoHeight          = 5
        , D.inactiveBorderColor = inactive
        , D.inactiveColor       = inactive
        , D.inactiveTextColor   = inactive
        , D.urgentBorderColor   = urgent
        , D.urgentTextColor     = urgent
        }

  active   = "#1273B8"
  inactive = "#000000"
  urgent   = "#F012BE"

  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 3 / 4
  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

delkeys :: XConfig a -> [(KeyMask, KeySym)]
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
  , ((modm,               xK_m), sendMessage Expand)
  , ((modm,               xK_i), sendMessage Shrink)
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
  -- Reflect layout across the x axis. There is a REFLECTY, but it flips the top
  -- highlight bar.
  , ((modm, xK_x), sendMessage $ MT.Toggle L.REFLECTX)
  -- rofi keybindings
  , ((controlMask              , xK_space), spawn "rofi -show combi -combi-modi 'drun,run,ssh' -modi combi -show-icons")
  , ((controlMask .|. shiftMask, xK_space), spawn "rofi -show p -modi p:rofi-power-menu -lines 6")
  ] ++
  -- This is using a list comprehension to build a list of workspace key
  -- bindings.
  [ ((modm .|. m, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys,
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

-- | This type is for adding a "smart" bar decoration style with the desired
-- theme and direction. The idea of "smart" I think comes from smart borders and
-- all it means is the bar is only visible when there's more than one window in
-- the current layout. I'm using this as an alternative to XMonad.Layout.IfMax
-- because of this bug: https://github.com/xmonad/xmonad-contrib/issues/75
--
-- This code comes from the following (I refactored and renamed a few things):
-- https://www.reddit.com/r/xmonad/comments/glkc6r/can_xmonad_apply_window_decorations_nofrillsdeco/fqy1vda/
-- https://github.com/disconsis/literate-xmonad-config/blob/53b6fdb72b5c2d13d84ef3b295e75257da7cdc8c/src/config.org
newtype SmartBarDeco a = SmartBarDeco Direction2D
  deriving (Eq, Show, Read)

instance Eq a => D.DecorationStyle SmartBarDeco a where
  shrink (SmartBarDeco direction) = shrinkWinForDeco direction
   where
    shrinkWinForDeco :: Direction2D -> Rectangle -> Rectangle -> Rectangle
    shrinkWinForDeco U (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + D.fi dh) w (h - D.fi dh)
    shrinkWinForDeco D (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x y w (h - D.fi dh)
    shrinkWinForDeco L (Rectangle _ _ dw _) (Rectangle x y w h) = Rectangle (x + D.fi dw) y (w - D.fi dw) h
    shrinkWinForDeco R (Rectangle _ _ dw _) (Rectangle x y w h) = Rectangle x y (w - D.fi dw) h

  pureDecoration (SmartBarDeco direction) decoWidth decoHeight _ _ windowRects (_win, Rectangle x y w h)
    | length windowRects >= 2
    = Just smartBarBar
    | otherwise
    = Nothing
   where
    smartBarBar = case direction of
      U -> Rectangle x y w decoHeight
      D -> Rectangle x (y + D.fi (h - decoHeight)) w decoHeight
      L -> Rectangle x y decoWidth h
      R -> Rectangle (x + D.fi (w - decoWidth)) y decoWidth h

smartBarDeco
  :: Eq a
  => Direction2D
  -> D.Theme
  -> l a
  -> ModifiedLayout (D.Decoration SmartBarDeco D.DefaultShrinker) l a
smartBarDeco direction theme =
  D.decoration D.shrinkText theme (SmartBarDeco direction)

{- |
   Module      : Data.Tree.PrettyPrint
   Description : Pretty-print trees.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

 -}
module Data.Tree.PrettyPrint
       ( drawVerticalTree
       , drawVerticalTreeWith
       , drawVerticalForest
       , drawVerticalForestWith
       , Width
       ) where

import Data.Tree
import Data.List(intersperse)
import Text.PrettyPrint.Boxes
import Control.Monad(ap, liftM2)

-- -----------------------------------------------------------------------------

type Width = Int

defaultGap :: Width
defaultGap = 2

data WidthLabel = WL { trWidth :: Width
                     , numSub  :: Int
                     , label   :: String
                     }
                deriving (Eq, Ord, Show, Read)

type WidthTree = Tree WidthLabel
type WidthForest = Forest WidthLabel

-- | Draw a tree top-down.
drawVerticalTree :: Tree String -> String
drawVerticalTree = drawVerticalTreeWith defaultGap

-- | Draw a tree top-down using the specified gap between sub-trees.
drawVerticalTreeWith    :: Width -> Tree String -> String
drawVerticalTreeWith gp = render . treeToBox gp

-- | Draw a forest with each tree being top-down.
drawVerticalForest :: Forest String -> String
drawVerticalForest = drawVerticalForestWith defaultGap

-- | Draw a forest with each tree being top-down and the specified
--    horizontal gap between trees.
drawVerticalForestWith    :: Width -> Forest String -> String
drawVerticalForestWith gp = render . hsep gp top . map (treeToBox gp)

checkGap :: Width -> Width
checkGap = max 1

treeToBox :: Width -> Tree String -> Box
treeToBox = liftM2 (.) treeBox addWidthTree . checkGap

addWidthTree :: Width -> Tree String -> WidthTree
addWidthTree gp (Node str ts) = Node (WL w ns str) ts'
  where
    ts' = addWidthsForest gp ts
    ns = length ts
    w = length str `max` forestWidth gp ts'

addWidthsForest    :: Width -> Forest String -> WidthForest
addWidthsForest gp = map (addWidthTree gp)

treeWidth :: WidthTree -> Width
treeWidth = trWidth . rootLabel

forestWidth    :: Width -> WidthForest -> Width
forestWidth gp = sum . intersperse gp . map treeWidth

drawLabel :: WidthLabel -> Box
drawLabel = text . label

-- | The width between the vertical lines coming into neighbouring sub-trees.
interTreeSpacing    :: Width -> WidthForest -> [Width]
interTreeSpacing gp = (zipWith go `ap` tail) . map (pred . trWidth . rootLabel)
  where
    go l r = (l `divUp` 2) + gp + (r `div` 2)

treeBox :: Width -> WidthTree -> Box
treeBox gp (Node lbl ts)
  = case ts of
      []  -> lbl'
             -- Three vLines to get the gap right.
      [t] -> vcat' [lbl', vLine, treeBox gp t]
      _   -> vcat' [lbl', vLine, lnTs]
  where
    numTs = numSub lbl

    lbl' = drawLabel lbl

    lnWidth = sum (interTreeSpacing gp ts) + numTs -- + the vertical lines

    iniGp = (`div` 2) . pred . trWidth . rootLabel $ head ts

    ln = moveRight (iniGp + 1) $ hcat top (replicate (lnWidth - 2) hLine)

    ts' = hsep gp top $ zipWith subT [1..] ts

    lnTs = ln // ts'

    subT n t = vcat' [vln, treeBox gp t]
      where
        vln | n == 1     = lBranch
            | n == numTs = rBranch
            | otherwise  = vLine



vLine :: Box
vLine = char '|'

hLine :: Box
hLine = char '-'

lBranch :: Box
lBranch = char '/'

rBranch :: Box
rBranch = char '\\'

divUp :: Int -> Int -> Int
a `divUp` b = negate $ (-a) `div` b

vcat' :: [Box] -> Box
vcat' = vcat center1

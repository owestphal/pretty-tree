{- |
   Module      : Data.Tree.Pretty
   Description : Pretty-print trees.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

"Data.Tree" exports 'drawTree' and 'drawForest', which provide 2D
pretty-printing of rose-trees, but in a left-to-right fashion.

The functions here draw trees more \"naturally\" in a top-down fashion.

For example, consider the following tree, based upon the GHC pipeline
shown in <http://www.aosabook.org/en/ghc.html#fig.ghc.pipeline> :

> ghcPhases :: Tree String
> ghcPhases = Node "<M.hs>"
>               [ Node "Parse"
>                   [ Node "Typecheck"
>                       [ Node "Desugar"
>                           [ Node "Simplify"
>                               [ Node "CoreTidy"
>                                   [ Node "CorePrep"
>                                       [ Node "Convert to STG"
>                                           [ Node "Code generation"
>                                               [ Node "Pretty-print C code"
>                                                   [ Node "<M.hc>" []]
>                                               , Node "Generate machine code"
>                                                   [ Node "<M.s>" []]
>                                               , Node "Generate LLVM code"
>                                                   [ Node "<M.ll>" []]]]]
>                                   , Node "Convert to IfaceSyn"
>                                       [ Node "Serialise"
>                                           [ Node "<M.hi>" []]]]]]]]]
>

Comparing 'drawTree' and 'drawVerticalTree' (note that the former is
"reflected" in the latter by a diagonal):

>>> putStrLn $ drawTree ghcPhases
<M.hs>
|
`- Parse
   |
   `- Typecheck
      |
      `- Desugar
         |
         `- Simplify
            |
            `- CoreTidy
               |
               +- CorePrep
               |  |
               |  `- Convert to STG
               |     |
               |     `- Code generation
               |        |
               |        +- Pretty-print C code
               |        |  |
               |        |  `- <M.hc>
               |        |
               |        +- Generate machine code
               |        |  |
               |        |  `- <M.s>
               |        |
               |        `- Generate LLVM code
               |           |
               |           `- <M.ll>
               |
               `- Convert to IfaceSyn
                  |
                  `- Serialise
                     |
                     `- <M.hi>

>>> putStrLn $ drawVerticalTree ghcPhases
                                                 <M.hs>
                                                   |
                                                 Parse
                                                   |
                                               Typecheck
                                                   |
                                                Desugar
                                                   |
                                                Simplify
                                                   |
                                                CoreTidy
                                                   |
                               ------------------------------------------
                              /                                          \
                           CorePrep                             Convert to IfaceSyn
                              |                                          |
                        Convert to STG                               Serialise
                              |                                          |
                       Code generation                                 <M.hi>
                              |
          ------------------------------------------
         /                     |                    \
Pretty-print C code  Generate machine code  Generate LLVM code
         |                     |                    |
       <M.hc>                <M.s>                <M.ll>

Also consider the @Diagrams.TwoD.Layout.Tree@ module from
/diagrams-contrib/ for actual image rendering of rose-trees:
<http://hackage.haskell.org/package/diagrams-contrib>

 -}
module Data.Tree.Pretty
       ( -- * Drawing trees
         drawVerticalTree
       , drawVerticalTreeWith
         -- * Drawing forests
       , drawVerticalForest
       , drawVerticalForestWith
         -- * Custom configuration
       , VTConfig(..)
       , defaultVTC
         -- * Helper functions
       , treeToBox
       , treeToBox'
       ) where

import Prelude hiding ((<>))

import Data.Tree

import Data.Maybe(listToMaybe)
import Text.PrettyPrint.Boxes
import Control.Monad(ap, liftM2)

-- -----------------------------------------------------------------------------

-- | Draw a tree top-down.
drawVerticalTree :: Tree String -> String
drawVerticalTree = drawVerticalTreeWith defaultVTC

-- | Draw a tree top-down using the specified gap between sub-trees.
drawVerticalTreeWith    :: VTConfig -> Tree String -> String
drawVerticalTreeWith cf = render . treeToBox cf

-- | Draw a forest with each tree being top-down.
drawVerticalForest :: Forest String -> String
drawVerticalForest = drawVerticalForestWith defaultVTC

-- | Draw a forest with each tree being top-down and the specified
--    horizontal gap between trees.
drawVerticalForestWith    :: VTConfig -> Forest String -> String
drawVerticalForestWith cf = render . hsep (treeGap cf) top . map (treeToBox cf)

checkGap    :: VTConfig -> VTConfig
checkGap cf = cf { treeGap = max 1 $ treeGap cf }

data VTConfig = VTC { -- | Specify whether to use \"prettier\" angled
                      --   lines for the first/last sub-tree or not.
                      --   You may wish to set this to 'False' if
                      --   using primarily binary trees with small
                      --   labels.
                      useAngledLines :: Bool
                      -- | The spacing to use between sub-trees.  It
                      --   is recommended that you use a value @>=2@
                      --   for best results.  If a value @<=0@ is
                      --   specified, then a width of @1@ is used.
                    , treeGap :: Int
                    }
              deriving (Eq, Ord, Show, Read)

-- | By default, use angled lines and a gap of @2@.
defaultVTC :: VTConfig
defaultVTC = VTC True 2

-- | This is exported in case you want to do further pretty-printing
--   using "Text.PrettyPrint.Boxes".
treeToBox    :: VTConfig -> Tree String -> Box
treeToBox cf = treeToBox' cf . fmap strToBox
  where
    strToBox= vcat left . map text . lines

-- | As with 'treeToBox', but allowing for more precise control over
--   label formatting (multiple lines, etc.).
treeToBox' :: VTConfig -> Tree Box -> Box
treeToBox' = liftM2 (.) treeBox addWidthTree . checkGap

-- -----------------------------------------------------------------------------
-- Pre-processing

type Width = Int

-- | We need to know how wide the tree is.
data WidthLabel = WL { leftWidth      :: !Width -- width left of bar from root
                     , rightWidth     :: !Width -- width right of bar from root
                       -- Tree width = leftWidth = rightWidth + 1 (for bar)
                     , lineWidth      :: !Width -- width of line for subtrees
                     , headerIndent   :: !Width
                     , subTreesIndent :: !Width -- For when labelWidth > lineWidth
                     , numSubTrees    :: !Int
                     , label          :: !Box
                     }
                deriving (Show)

type WidthTree = Tree WidthLabel
type WidthForest = Forest WidthLabel

addWidthTree :: VTConfig -> Tree Box -> WidthTree
addWidthTree cf (Node str ts) = Node lbl ts'
  where
    ts' = addWidthsForest cf ts
    numTs = length ts
    lblW = cols str
    lblWL = widthLeft lblW
    lnW = sum (interTreeSpacing cf ts') + numTs -- + the vertical lines
    lnWL = widthLeft lnW
    hdW = lblW `max` lnW
    wLeftOfLine = maybe 0 (leftWidth . rootLabel) $ listToMaybe ts'
    subWL = wLeftOfLine + lnWL

    hdInd = nonNeg $ subWL - widthLeft hdW
    stInd = nonNeg $ lblWL - subWL

    wRightOfLine = maybe 0 (rightWidth . rootLabel) . listToMaybe $ reverse ts'

    lbl = WL { leftWidth      = lblWL `max` subWL
             , rightWidth     = (widthRight lblW) `max` (widthRight lnW + wRightOfLine)
             , lineWidth      = lnW
             , headerIndent   = hdInd
             , subTreesIndent = stInd
             , numSubTrees    = numTs
             , label          = str
             }

addWidthsForest    :: VTConfig -> Forest Box -> WidthForest
addWidthsForest cf = map (addWidthTree cf)

widthLeft :: Width -> Width
widthLeft = (`div` 2) . pred

widthRight :: Width -> Width
widthRight = (`divUp` 2) . pred

-- | The width between the vertical lines coming into neighbouring sub-trees.
interTreeSpacing    :: VTConfig -> WidthForest -> [Width]
interTreeSpacing cf = (zipWith go `ap` tail) . map rootLabel
  where
    go lt rt = rightWidth lt + treeGap cf + leftWidth rt

-- -----------------------------------------------------------------------------
-- Drawing

treeBox :: VTConfig -> WidthTree -> Box
treeBox cf (Node lbl ts)
  = case ts of
      []  -> lbl'
      _   -> hdr // ts'
  where
    numTs = numSubTrees lbl

    lbl' = label lbl

    angled = useAngledLines cf

    lnWidth = lineWidth lbl

    ln
      | numTs == 1 = nullBox
      | angled     = space <> hcat top (replicate (lnWidth - 2) hLine) <> space
      | otherwise  = hcat top $ replicate lnWidth hLine

    ts' = moveRight (subTreesIndent lbl)
          . hsep (treeGap cf) top $ zipWith subT [1..] ts

    hdr = moveRight (headerIndent lbl) $ vcat' [lbl', vLine, ln]

    subT n t = vln' // treeBox cf t
      where
        vln | numTs == 1 = nullBox
            | not angled = vLine
            | n == 1     = lBranch
            | n == numTs = rBranch
            | otherwise  = vLine

        vln' = moveRight (leftWidth $ rootLabel t) vln

-- -----------------------------------------------------------------------------

vLine :: Box
vLine = char '|'

hLine :: Box
hLine = char '-'

lBranch :: Box
lBranch = char '/'

rBranch :: Box
rBranch = char '\\'

space :: Box
space = emptyBox 1 1

vcat' :: [Box] -> Box
vcat' = vcat center1

divUp :: Int -> Int -> Int
a `divUp` b = negate $ (-a) `div` b

nonNeg :: Int -> Int
nonNeg = max 0

_ghcPhases :: Tree String
_ghcPhases = Node "<M.hs>"
               [ Node "Parse"
                   [ Node "Typecheck"
                       [ Node "Desugar"
                           [ Node "Simplify"
                               [ Node "CoreTidy"
                                   [ Node "CorePrep"
                                       [ Node "Convert to STG"
                                           [ Node "Code generation"
                                               [ Node "Pretty-print C code"
                                                   [ Node "<M.hc>" []]
                                               , Node "Generate machine code"
                                                   [ Node "<M.s>" []]
                                               , Node "Generate LLVM code"
                                                   [ Node "<M.ll>" []]]]]
                                   , Node "Convert to IfaceSyn"
                                       [ Node "Serialise"
                                           [ Node "<M.hi>" []]]]]]]]]

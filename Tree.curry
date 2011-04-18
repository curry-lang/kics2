-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Multi-way trees (/aka/ rose trees) and forests.
--
-----------------------------------------------------------------------------

module Tree
  ( Tree(..), Forest
    -- * Two-dimensional drawing
  , drawTree, drawForest
    -- * Extraction
  , flatten, levels
    -- * Building trees
  , unfoldTree, unfoldForest
  ) where

-- | Multi-way trees, also known as /rose trees/.
data Tree a = Node a (Forest a)

type Forest a = [Tree a]

rootLabel (Node x _)  = x
subForest (Node _ ts) = ts

mapTree              :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Node x ts) = Node (f x) (map (mapTree f) ts)

-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees []  = []
        drawSubTrees [t] =
                "|" : shift "`- " "   " (draw t)
        drawSubTrees (t:t2:ts) =
                "|" : shift "+- " "|  " (draw t) ++ drawSubTrees (t2:ts)

        shift first other = zipWith (++) (first : repeat other)

-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t = map (map rootLabel) $
                takeWhile (not . null) $
                iterate (concatMap subForest) [t]

-- | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
unfoldForest f = map (unfoldTree f)

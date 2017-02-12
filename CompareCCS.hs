{-# LANGUAGE ScopedTypeVariables #-}

import Data.Semigroup
import Data.Either
import Data.Maybe
import Data.Function
import Data.List (sortBy)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy as HM
import qualified Data.DList as DList
import Data.Hashable
import Data.Tree
import GHC.CCS.JSON

main :: IO ()
main = do
    Just a <- Json.decode <$> BSL.readFile "before.prof"
    Just b <- Json.decode <$> BSL.readFile "after.prof"

    let toDelta (a,b) = ( costCentreLabel $ stackCostCentre a
                        , (relAllocDelta `on` stackStats) a b)
    putStrLn
        $ unlines $ map show
        $ sortBy (flip (compare `on` snd . snd))
        $ foldMap (flattenWithPath fst)
        $ map (fmap toDelta)
        $ filterBoth
        $ zipProfiles [profileStack a] [profileStack b]

    --let showCCSs (a,b) = show (costCentreLabel $ stackCostCentre a) <> ": " <> show (stackStats a) <> "\t" <> show (stackStats b) <> "\t" <> show (relAllocDelta (stackStats a) (stackStats b))
    --putStrLn $ drawForest $ map (fmap showCCSs) $ filterBoth
    --    $ zipProfiles [profileStack a] [profileStack b]

relAllocDelta :: Stats -> Stats -> Double
relAllocDelta a b = (realToFrac (allocs b) - realToFrac (allocs a)) / realToFrac (allocs a)

-- | Flatten a tree, retaining some breadcrumbs from its paths.
flattenWithPath :: forall a p. (a -> p) -> Tree a -> [([p], a)]
flattenWithPath toPath = go mempty
  where
    go :: DList.DList p -> Tree a -> [([p], a)]
    go accum (Node x children)=
        (DList.toList accum, x) : foldMap (go accum') children
      where
        accum' = accum `DList.snoc` toPath x

-- | Sort all levels of a forest with the given ordering.
sortForestWith :: (a -> a -> Ordering) -> Forest a -> Forest a
sortForestWith cmp =
    sortBy (cmp `on` rootLabel)
    . map (\(Node n cs) -> Node n (sortForestWith cmp cs))

zipProfiles :: Forest CCS -> Forest CCS -> [ZippedTree CCS CCS]
zipProfiles = zipForests key key
  where key = costCentreLabel . stackCostCentre

-- | Produce a 'Forest' of the 'InBoth' trees from a 'zipForests' result.
filterBoth :: [ZippedTree a b] -> Forest (a,b)
filterBoth = mapMaybe go
  where
    go (InBoth x y cs) = Just $ Node (x,y) (filterBoth cs)
    go _ = Nothing

filterOnly :: [ZippedTree a b] -> ([Tree a], [Tree b])
filterOnly = partitionEithers . mapMaybe go
  where
    go (OnlyLeft x)  = Just $ Left x
    go (OnlyRight x) = Just $ Right x

data ZippedTree a b = OnlyLeft (Tree a)
                    | OnlyRight (Tree b)
                    | InBoth a b [ZippedTree a b]
                    deriving (Show)

-- | Zip two forests together.
zipForests :: forall a b k. (Hashable k, Eq k)
           => (a -> k) -> (b -> k)
           -> Forest a -> Forest b
           -> [ZippedTree a b]
zipForests f g = go
  where
    go :: Forest a -> Forest b -> [ZippedTree a b]
    go xs ys =
        HM.elems (HM.intersectionWith doBoth xs' ys')
        <> map (\(x, cs) -> OnlyLeft  $ Node x cs) (HM.elems $ HM.difference xs' ys')
        <> map (\(x, cs) -> OnlyRight $ Node x cs) (HM.elems $ HM.difference ys' xs')
      where
        xs' = group f xs
        ys' = group g ys

    doBoth :: (a, Forest a) -> (b, Forest b) -> ZippedTree a b
    doBoth (x, xs) (y, ys) = InBoth x y (go xs ys)

    group :: forall x. (x -> k) -> Forest x -> HM.HashMap k (x, Forest x)
    group toKey xs = HM.fromList [ (toKey x, (x, xs')) | Node x xs'<- xs ]

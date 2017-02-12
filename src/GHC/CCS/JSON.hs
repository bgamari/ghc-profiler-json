{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.CCS.JSON
    ( CostCentreId
    , CostCentre(..)
    , ProfileMeta(..)
    , Stats(..)
    , CCS(..)
    , inherit
    , Profile(..)
    ) where

import Data.Semigroup
import Data.Word
import Data.Hashable
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.IntMap as IM
import qualified Data.Tree as Tree

newtype CostCentreId = CCID Int
                     deriving (Show, Eq, Ord, Enum, FromJSON, ToJSON, Hashable)

newtype ModuleName = ModuleName T.Text
                   deriving (Show, Eq, Ord, FromJSON, ToJSON, Hashable)

data CostCentre = CostCentre { costCentreId     :: !CostCentreId
                             , costCentreLabel  :: T.Text
                             , costCentreModule :: ModuleName
                             , costCentreSrcLoc :: T.Text
                             , costCentreIsCaf  :: Bool
                             }
                deriving (Show)

instance FromJSON CostCentre where
    parseJSON = withObject "cost centre" $ \o ->
      CostCentre
        <$> o .: "id"
        <*> o .: "label"
        <*> o .: "module"
        <*> o .: "src_loc"
        <*> o .: "is_caf"

-- | Metadata about an execution of a profiled program.
data ProfileMeta = ProfileMeta { program      :: T.Text
                               , arguments    :: [T.Text]
                               , rtsArguments :: [T.Text]
                               , totalTime    :: Double
                               , totalTicks   :: Int
                               , tickInterval :: Int
                               , totalAlloc   :: Word64
                               }
                 deriving (Show)

data Stats = Stats { allocs  :: !Word
                   , ticks   :: !Word
                   }
           deriving (Show)

instance Monoid Stats where
    mempty = Stats 0 0
    mappend = (<>)

instance Semigroup Stats where
    Stats a b <> Stats x y = Stats (a + x) (b + y)

-- | Cost center stack
data CCS = CCS { stackCostCentre :: CostCentre
               , stackEntries    :: !Int
               , stackStats      :: !Stats
               }
         deriving (Show)

inherit :: Tree.Tree CCS -> Tree.Tree CCS
inherit (Tree.Node (CCS cc entries stats) children) =
    Tree.Node (CCS cc entries stats') children'
  where
    children' = fmap inherit children
    stats' = stats <> foldMap (stackStats . Tree.rootLabel) children'

parseCCS :: (CostCentreId -> CostCentre) -> Value -> Parser (Tree.Tree CCS)
parseCCS lookupCcid = go
  where
    go = withObject "CCS" $ \o -> do
        stats <- Stats
                 <$> o .: "alloc"
                 <*> o .: "ticks"
        n <- CCS
             <$> fmap lookupCcid (o .: "id")
             <*> o .: "entries"
             <*> pure stats
        children <- o .: "children" >>= mapM go
        return $ Tree.Node n children

-- | A profile from GHC's cost-centre profiler.
data Profile = Profile { profileMeta  :: ProfileMeta
                       , profileStack :: Tree.Tree CCS
                       }
             deriving (Show)

instance FromJSON Profile where
    parseJSON = withObject "profile" $ \o -> do
        meta <- ProfileMeta
          <$> o .: "program"
          <*> o .: "arguments"
          <*> o .: "rts_arguments"
          <*> o .: "total_time"
          <*> o .: "total_ticks"
          <*> o .: "tick_interval"
          <*> o .: "total_alloc"
        ccs <- o .: "cost_centres"
        let cc_map = IM.fromList [ (ccid, cc)
                                 | cc <- ccs
                                 , CCID ccid <- pure (costCentreId cc)
                                 ]
            lookupCCID (CCID ccid)
              | Just cc <- ccid `IM.lookup` cc_map = cc
              | otherwise                          = error "Unknown cost-centre ID"
        stack <- o .: "profile" >>= parseCCS lookupCCID
        return $ Profile meta stack

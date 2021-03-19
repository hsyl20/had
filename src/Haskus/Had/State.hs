module Haskus.Had.State where

import qualified Data.Map.Lazy as LMap
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Numeric.Natural

import Haskus.Had.CmdLine

data State = State
   { stateNotes       :: LMap.Map ID Note -- ^ All the notes (Map commit note)
   , stateLastCommits :: [(ID,Text)]      -- ^ Last commits and their summary
   , stateTestIds     :: [TestId]         -- ^ All the test IDs we've found
   , stateRunners     :: [Runner]         -- ^ All the runners we've found
   , stateOpts        :: Options          -- ^ Input options
   }

data Note = Note
   { noteId    :: ID
   , noteTests :: Map Runner (Map TestId Natural)
   }

type Runner = Text

data TestId = TestId
   { testName   :: Text
   , testWay    :: Text -- ^ "normal", "optasm", etc.
   , testMetric :: Metric
   }
   deriving (Show,Eq,Ord,Read)

data Metric
   = Metric !MetricTime !MetricDesc
   deriving (Show,Eq,Ord,Read)

data MetricDesc
   = BytesAlloc
   | PeakMegabytesAlloc
   | MaxBytesUsed
   deriving (Show,Eq,Ord,Read)

data MetricTime
   = CompileTime
   | Runtime
   deriving (Show,Eq,Ord,Read)

type ID = Text


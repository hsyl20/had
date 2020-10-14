{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import System.Process.Typed

import Data.Char
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS (readInteger)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as LMap
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Lucid
import Numeric.Natural
import Data.FileEmbed
import Data.Maybe
import qualified Data.List as List

chartJs :: BS.ByteString
chartJs = $(embedFile "static/Chart.bundle.min.js")

chartCss :: BS.ByteString
chartCss = $(embedFile "static/Chart.css")

data State = State
   { stateNotes       :: LMap.Map ID Note -- ^ All the notes (Map commit note)
   , stateLastCommits :: [(ID,Text)]      -- ^ Last commits and their summary
   , stateTestIds     :: [TestId]         -- ^ All the test IDs we've found
   , stateRunners     :: [Runner]         -- ^ All the runners we've found
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

showTestId :: TestId -> Text
showTestId tid = mconcat
   [ testName tid
   , " / "
   , testWay tid
   , " / "
   , showMetric (testMetric tid)
   ]

showMetric :: Metric -> Text
showMetric (Metric t d) = mconcat
   [ case t of
      CompileTime -> "ghc"
      Runtime     -> "test"
   , " / "
   , case d of
      BytesAlloc         -> "bytes allocated"
      PeakMegabytesAlloc -> "peak megabytes"
      MaxBytesUsed       -> "max bytes used"
   ]

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

main :: IO ()
main = do
   putStrLn $ "http://localhost:8080/"

   putStrLn "Reading current state..."
   state <- newState 2000
   mstate <- newMVar state
   putStrLn "Done."
   run 8080 (app mstate)

newState :: Word -> IO State
newState ncommits = do
   --putStrLn "Fetch origin..."
   --gitFetchOrigin
   putStrLn $ "Get latest " <> show ncommits <> " commits..."
   latest_ids <- gitLastCommits ncommits
   summaries <- forM latest_ids gitObjectSummary
   let latests = latest_ids `zip` summaries

   putStrLn "Fetch perf notes..."
   gitUpdateNotes
   putStrLn "Read perf notes..."
   notes_map <- gitReadNotesMap
   let !latest_ids_set = Set.fromList latest_ids
   let !latest_notes_map = Map.restrictKeys notes_map latest_ids_set
   notes <- forM latest_notes_map $ \nid -> do
      res <- gitShowObject nid
      return $ parseNote nid res

   putStrLn "Gather all test IDs..."
   let !test_ids = List.nub
                     $ List.sort
                     $ concat $ concat
                     $ fmap (fmap Map.keys . Map.elems . noteTests)
                     $ Map.elems notes

   putStrLn "Gather all runners..."
   let -- some quotes have been added by mistake then removed on some runners names
       fix_runner t
         | Just x <- Text.stripPrefix "\"" t
         , Just y <- Text.stripSuffix "\"" x
         = y
         | otherwise
         = t
       !runners = List.nub
                     $ List.sort
                     $ fmap fix_runner
                     $ concat
                     $ fmap (Map.keys . noteTests)
                     $ Map.elems notes

   let state = State
        { stateNotes       = notes
        , stateLastCommits = latests
        , stateTestIds     = test_ids
        , stateRunners     = runners
        }
   putStrLn "Update done."
   return state

app :: MVar State -> Application
app mstate request respond = case pathInfo request of
   [] -> do
      state <- readMVar mstate
      let html = do
            h1_ "Runners"
            renderAllRunners state
            h1_ "Misc"
            a_ [href_ "/commits"] "Recent commits"
      respond $ htmlResponse html

   ["commits"] -> do
      state <- readMVar mstate
      let html = renderCommitList state
      respond $ htmlResponse html

   ["commit",cid] -> do
      state <- readMVar mstate
      let cid' = Text.fromStrict cid
      summary <- gitObjectSummary cid'
      let html = renderCommit state (cid',summary)
      respond $ htmlResponse html

   ["script","chart.js"] -> do
      respond $ responseLBS
         status200
         [("Content-Type", "text/javascript")]
         (LBS.fromStrict chartJs)

   ["style","chart.css"] -> do
      respond $ responseLBS
         status200
         [("Content-Type", "text/css")]
         (LBS.fromStrict chartCss)

   ["show",obj] -> do
      res <- gitShowObject obj
      respond $ responseLBS
         status200
         [("Content-Type", "text/plain")]
         res

   ["note",obj] -> do
      res <- gitShowObject obj
      let note = parseNote (Text.fromStrict obj) res
          html = renderNote note
      respond $ htmlResponse html

   ["chart",runner] -> do
      state <- readMVar mstate
      let html = renderCommitChart state (Text.fromStrict runner)
      respond $ htmlResponse html

   _ -> respond $ notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

-- | Add HTML headers and stuff
htmlWrap :: Html () -> Html ()
htmlWrap body = do
   html_ do
      head_ do
         title_ "HAD"
         meta_ [ httpEquiv_ "Content-Type"
               , content_   "text/html;charset=utf-8"
               ]

         -- chart.js
         script_ [src_ "/script/chart.js"] ("" :: String)
         link_ [ rel_  "stylesheet"
               , type_ "text/css"
               , href_ "style/chart.css"
               ]
      body_ [style_ "padding: 0px; margin: 0px"] body


htmlResponse :: Html () -> Response
htmlResponse html = responseLBS status200 [("Content-Type","text/html")] (renderBS (htmlWrap html))


parseNote :: ID -> ByteString -> Note
parseNote nid bs = Note nid
   $ Map.fromListWith (Map.union)
   $ fmap (\[r,n,w,m,v] -> (Text.decodeUtf8 r, Map.singleton
                                 ( TestId
                                    { testName = Text.decodeUtf8 n
                                    , testWay  = Text.decodeUtf8 w
                                    , testMetric = case LBS.split (fromIntegral (ord '/')) m of
                                          [tim,desc] -> Metric
                                             (case tim of
                                                "runtime"      -> Runtime
                                                "compile_time" -> CompileTime
                                                _              -> error ("Invalid metric time: " <> show tim))
                                             (case desc of
                                                "bytes allocated"          -> BytesAlloc
                                                "max_bytes_used"           -> MaxBytesUsed
                                                "peak_megabytes_allocated" -> PeakMegabytesAlloc
                                                _                          -> error ("Invalid metric desc: " <> show desc))
                                          _         -> error ("Invalid metric: " <> show m <> ", ID: " <> show nid)
                                    })
                                 ( case LBS.readInteger v of
                                    Nothing -> error ("Invalid metric value: " <> show v)
                                    Just (x,_)  -> fromIntegral x
                                 )))
   $ filter (not . null)
   $ fmap (LBS.split (fromIntegral (ord '\t')))
   $ LBS.split (fromIntegral (ord '\n')) bs


--------------------------------------
-- Rendering
--------------------------------------

renderCommitList :: State -> Html ()
renderCommitList state = do
   let ids   = stateLastCommits state
   forM_ ids $ \c -> do
      renderCommit state c
      hr_ []

renderCommit :: State -> (ID,Text) -> Html ()
renderCommit state (cid,summary) = do
   let notes = stateNotes state
   pre_ do
      toHtml summary
   div_ do
      a_ [href_ $ "/show/" <> Text.toStrict cid] $ "Diff"
      " - "
      case Map.lookup cid notes of
         Just (Note note_id _) -> do
            a_ [href_ $ "/note/" <> Text.toStrict note_id] $ "Perf report"
         Nothing -> "Perf report not available"
      " - "
      a_ [ href_ $ "https://gitlab.haskell.org/ghc/ghc/-/commit/" <> Text.toStrict cid
         , target_ "_blank"
         ] $ "Gitlab"


renderAllRunners :: State -> Html ()
renderAllRunners state = do
   ul_ $ forM_ (stateRunners state) \runner -> do
      li_ $ a_ [href_ $ "/chart/" <> Text.toStrict runner] (toHtml runner)

renderCommitChart :: State -> Runner -> Html ()
renderCommitChart state runner = do
   let notes  = stateNotes state
       ids    = fmap fst (stateLastCommits state)
       tids   = stateTestIds state

   script_ $ Text.toStrict $
      "function customToolTips(tooltip) {\n\
      \  var tt = document.getElementById('tooltip');\n\
      \  if (!tooltip || !tooltip.dataPoints) {\n\
      \     //tt.innerHTML = '';\n\
      \  }\n\
      \  else {\n\
      \     var data = tooltip.dataPoints[0];\n\
      \     fetch('/commit/'+data.label).then((res) => { return res.text(); }).then((html) => tt.innerHTML = \"Value: \" + data.value + \"<br/><br/>\" + html);\n\
      \  }\n\
      \}"

   let
       leftPane =  do
         h2_ (toHtml runner)

         forM_ tids $ \tid -> do
            let chart_id = "chart-" <> showTestId tid <> "-" <> runner
                lookup_value cid = case Map.lookup cid notes of
                  Nothing   -> Nothing -- no perf report for the commit
                  Just note -> case Map.lookup runner (noteTests note) of
                     Nothing -> Nothing -- no result for this runner
                     Just ts -> case Map.lookup tid ts of
                        Nothing -> Nothing -- no result for this test
                        Just v  -> Just (cid,v)
                labelledValues = reverse $ catMaybes (fmap lookup_value ids)
                valueName = showTestId tid

            unless (null labelledValues) do

               let
                  -- filter successive values with less than 1% change
                  go _ []     = []
                  go _ [b]    = [b] -- keep the last value
                  go a (b:bs)
                     | valueA <- fromIntegral (snd a)
                     , valueB <- fromIntegral (snd b)
                     , abs ((valueA - valueB) / (valueA :: Rational)) > 0.005 -- > 0.5%
                     = a : go b bs
                     | otherwise
                     = go a bs -- keep "a" as the baseline

                  --(labels,values) = unzip labelledValues
                  -- filter interesting commits
                  (labels,values) = unzip $ head labelledValues : go (head labelledValues) (tail labelledValues)
               h3_ (toHtml (showTestId tid))
               div_ $ canvas_
                  [ id_ (Text.toStrict chart_id)
                  , style_ "width: 50em;"
                  ] mempty
               script_ $ Text.toStrict $
                  "new Chart(document.getElementById('" <> chart_id <> "'), {\
                  \   type: 'line',\
                  \   data: {\
                  \      labels: " <> Text.pack (show labels) <> ",\
                  \      datasets: [{\
                  \         label: '"<> valueName <> "',\
                  \         data: " <> Text.pack (show values) <> ",\
                  \         steppedLine: 'before',\
                  \         fill: false,\
                  \         borderColor: 'rgba(255,0,0,0.5)',\
                  \         backgroundColor: 'rgba(255,0,0,0.5)'\
                  \         }]\
                  \   },\
                  \   options: {\
                  \      legend: { display: false},\
                  \      animation: { duration: 0},\
                  \      maintainAspectRatio: false,\
                  \      scales: {\
                  \         yAxes: [{ type: 'linear', ticks: {beginAtZero: true}}],\
                  \         xAxes: [{ ticks: { callback: function(value,index,values) {\
                  \           return value.substring(0,7);}}}]\
                  \      },\
                  \      tooltips: {\
                  \        mode: 'point',\
                  \        custom: customToolTips,\
                  \        enabled: false\
                  \      }\
                  \   }\
                  \});"

   div_ [ style_ "display: inline-flex; width: 100vw"] do
      div_ [ id_ "leftPane"
           , style_ "margin:0px; height: 100vh; flex-basis:70vw; overflow-y: scroll; resize:horizontal"
           ] leftPane
      div_ [ id_ "rightPane"
           , style_ "height: 100vh; flex-basis:30vw; overflow-y: auto; background-color:antiquewhite; resize:horizontal"
           ] do
         div_ [ id_ "tooltip"
              , style_ "width: 100%"
              ] ""


renderNote :: Note -> Html ()
renderNote note = do
   forM_ (Map.toList (noteTests note)) $ \(runner,tests) -> do
      h2_ (toHtml runner)
      let chart_id = "chart-" <> runner
      div_ $ canvas_
         [ id_ (Text.toStrict chart_id)
         , style_ $ "width: 95%; height: " <> (Text.toStrict $ Text.pack $ show $ fromIntegral (length tests) * (1.2 :: Float)) <> "em"
         ] mempty
      let chrt = "new Chart(document.getElementById('" <> chart_id <> "'), {\
                 \   type: 'horizontalBar',\
                 \   data: {\
                 \      labels: " <> Text.pack (show (fmap showTestId (Map.keys tests))) <> ",\
                 \      datasets: [{\
                 \         label: 'Value',\
                 \         data: " <> Text.pack (show (Map.elems tests)) <> ",\
                 \         backgroundColor: 'rgba(255,0,0,0.5)'\
                 \         }]\
                 \   },\
                 \   options: {\
                 \      legend: { display: false},\
                 \      animation: { duration: 0},\
                 \      maintainAspectRatio: false,\
                 \      scales: {\
                 \         xAxes: [{ type: 'logarithmic'}]\
                 \      }\
                 \   }\
                 \});"
      script_ (Text.toStrict chrt)

--------------------------------------
-- Git stuff
--------------------------------------

-- | Read performance notes from Git
gitReadNotesMap :: IO (Map ID ID)
gitReadNotesMap = do
   (_exitCode,res,_err) <- readProcess "git notes --ref=ci/perf"
   return $ Map.fromList
          $ fmap (\[a,b] -> (Text.decodeUtf8 b, Text.decodeUtf8 a)) -- one note per commit?
          $ filter (not . null)
          $ fmap (LBS.split (fromIntegral (ord ' ')))
          $ LBS.split (fromIntegral (ord '\n')) res

-- | Update notes from Gitlab
gitUpdateNotes :: IO ()
gitUpdateNotes = do
   let cmd = shell ("git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf")
   (_exitCode,_res,_err) <- readProcess cmd
   return ()

-- | Update branch from origin
gitFetchOrigin :: IO ()
gitFetchOrigin = do
   let cmd = shell ("git fetch origin")
   (_exitCode,_res,_err) <- readProcess cmd
   return ()

-- | Return latest N commit IDs from "origin/master"
gitLastCommits :: Word -> IO [ID]
gitLastCommits n = do
   let cmd = shell ("git log --pretty=%H origin/master -" <> show n)
   (_exitCode,res,_err) <- readProcess cmd
   return $ fmap Text.decodeUtf8 $ LBS.split (fromIntegral (ord '\n')) res

gitShowObject :: Show a => a -> IO ByteString
gitShowObject obj = do
   (_exitCode,res,_err) <- readProcess (shell ("git show " <> show obj))
   return res

gitObjectSummary :: ID -> IO Text
gitObjectSummary obj = do
   let cmd = shell ("git show --summary --decorate=no " <> show obj)
   (_exitCode,res,_err) <- readProcess cmd
   return $ Text.decodeUtf8 res

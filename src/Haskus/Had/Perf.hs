{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Haskus.Had.Perf where

import Data.Char
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS (readInteger)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Lucid
import Data.Bifunctor
import Data.Maybe

import Haskus.Had.State
import Haskus.Had.CmdLine

change_ratio :: Double
change_ratio = 0.005 -- 0.5%

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

encodeTestId :: TestId -> Text
encodeTestId tid = mconcat
   [ testName tid
   , "_"
   , testWay tid
   , "_"
   , encodeMetric (testMetric tid)
   ]

encodeMetric :: Metric -> Text
encodeMetric (Metric t d) = mconcat
   [ case t of
      CompileTime -> "ghc"
      Runtime     -> "test"
   , "_"
   , case d of
      BytesAlloc         -> "alloc"
      PeakMegabytesAlloc -> "peak"
      MaxBytesUsed       -> "max"
   ]

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
      \     fetch('/commit/'+data.label).then((res) => { return res.text(); }).then((html) => tt.innerHTML = \"Value: \" + data.value + \"<hr/>\" + html);\n\
      \  }\n\
      \}"

   let
       leftPane =  do
         h2_ (toHtml runner)

         forM_ tids $ \tid -> do
            let chart_id = "chart-" <> showTestId tid <> "-" <> runner
                nan = 0/0 :: Double
                lookup_value cid = case Map.lookup cid notes of
                  Nothing   -> Nothing -- no perf report for the commit
                  Just note -> case Map.lookup runner (noteTests note) of
                     Nothing -> Nothing -- no result for this runner
                     Just ts -> case Map.lookup tid ts of
                        Nothing -> Nothing -- no result for this test
                        Just v  -> Just (fromIntegral v)
                labelledValues = reverse
                                    $ fmap (second (fromMaybe nan))
                                    -- $ filter (not. isNothing . snd) -- filter missing values because there are too many...
                                    $ fmap (\x -> (x, lookup_value x)) ids
                valueName = showTestId tid


            let
               -- filter successive values with less than change_ratio change
               go _ _ []     = []
               go _ _ [b]    = [b] -- keep the last value
               go ref _lst (b:bs)
                  | isNaN (snd ref) && isNaN (snd b)
                  = go ref Nothing bs -- only keep one missing value
               go ref _lst (b:bs)
                  | isNaN (snd ref)
                  = b : go b Nothing bs
               go _ mlst (b:bs)
                  | isNaN (snd b)
                  = case mlst of
                     Nothing  -> b : go b Nothing bs
                     Just lst -> lst : b : go b Nothing bs
               go ref _lst (b:bs)
                  | valueA <- snd ref
                  , valueB <- snd b
                  , abs ((valueA - valueB) / valueA) > change_ratio
                  = b : go b Nothing bs
                  | otherwise
                  = go ref (Just b) bs -- keep "ref" as the baseline to avoid drift

               --(labels,values) = unzip labelledValues
               -- filter interesting commits
               (labels,values)
                  | null labelledValues = ([],[])
                  | otherwise =
                     let h = head labelledValues
                     in unzip $ h : go h Nothing (tail labelledValues)

            unless (null values || all isNaN values) do

               let ttid = showTestId tid
                   stid = encodeTestId tid
               a_ [ name_ (Text.toStrict stid)
                  , href_ ("#"<> Text.toStrict stid)
                  , style_ "color: gray; text-decoration: none"
                  ] do
                  h3_ (toHtml ttid)
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
         "Notes:"
         ul_ do
            li_ do
               "Only the latest "
               toHtml (show (opt_ncommits (stateOpts state)))
               " commits are taken into account."
            li_ do
               "Only commits with absolute metric change > "
               toHtml (show (change_ratio * 100))
               "% are displayed in continuous series"
            li_ do
               "Discontinuities indicate missing values. You can see commits without perf reports "
               a_ [href_ "/commits"] "here"
         hr_ []
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

parseNote :: ID -> ByteString -> Note
parseNote nid bs = Note nid
   $ Map.fromListWith (Map.union)
   $ fmap (\case
      [r,n,w,m,v] -> (Text.decodeUtf8 r, Map.singleton
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
                                 ))
      xs -> error $ "Unexpected perf note format: " ++ show xs
      )
   $ filter (not . null)
   $ fmap (LBS.split (fromIntegral (ord '\t')))
   $ LBS.split (fromIntegral (ord '\n')) bs



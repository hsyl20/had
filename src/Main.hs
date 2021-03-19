{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Lucid
import Data.FileEmbed
import qualified Data.List as List

import Haskus.Had.State
import Haskus.Had.Perf
import Haskus.Had.CmdLine
import Haskus.Had.Git
import Haskus.Had.Misc

chartJs :: BS.ByteString
chartJs = $(embedFile "static/Chart.bundle.min.js")

chartCss :: BS.ByteString
chartCss = $(embedFile "static/Chart.css")

styleCss :: BS.ByteString
styleCss = $(embedFile "static/style.css")

main :: IO ()
main = do
   opts <- getOptions

   putStrLn "Reading current state..."
   state <- newState opts
   mstate <- newMVar state

   putStrLn $ "Starting server on: http://localhost:" ++ show (opt_port opts) ++ "/"
   run (opt_port opts) (app mstate)

newState :: Options -> IO State
newState opts = do
   --putStrLn "Fetch origin..."
   --gitFetchOrigin
   let ncommits = opt_ncommits opts
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
        , stateOpts        = opts
        }
   putStrLn "Update done."
   return state

app :: MVar State -> Application
app mstate request respond = case pathInfo request of
   [] -> do
      state <- readMVar mstate
      let menu = 
              [ MenuEntry "#" "Runners"
              , MenuEntry "#" "Commits"
              ]
          cards =
              [ Card Full (renderAllRunners state)
              , Card Default $ a_ [href_ "/commits"] "Recent commits"
              ]
      let html = do
            layout menu cards
            --welcome
            --widget "CI runners" do
            --  renderAllRunners state
            --widget "Commits" do
            --  a_ [href_ "/commits"] "Recent commits"
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

   ["style","style.css"] -> do
      respond $ responseLBS
         status200
         [("Content-Type", "text/css")]
         (LBS.fromStrict styleCss)

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
         link_ [ rel_  "stylesheet"
               , type_ "text/css"
               , href_ "/style/style.css"
               ]

         -- chart.js
         script_ [src_ "/script/chart.js"] ("" :: String)
         link_ [ rel_  "stylesheet"
               , type_ "text/css"
               , href_ "/style/chart.css"
               ]

      body_ [style_ "padding: 0px; margin: 0px"] body


htmlResponse :: Html () -> Response
htmlResponse html = responseLBS status200 [("Content-Type","text/html")] (renderBS (htmlWrap html))

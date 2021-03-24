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
import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import Data.Text (Text)
import Lucid
import Data.FileEmbed
import qualified Data.List as List

import System.Environment

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import qualified GitLab

import Haskus.Had.State
import Haskus.Had.Perf
import Haskus.Had.CmdLine
import Haskus.Had.Git
import Haskus.Had.Misc
import Haskus.Had.GitLab
import Haskus.Had.Cards

chartJs :: BS.ByteString
chartJs = $(embedFile "static/Chart.bundle.min.js")

chartCss :: BS.ByteString
chartCss = $(embedFile "static/Chart.css")

styleCss :: BS.ByteString
styleCss = $(embedFile "static/style.css")

main :: IO ()
main = do
   let tok_env = "HAD_GITLAB_TOKEN"
   token <- Text.pack <$> getEnv tok_env
   unsetEnv tok_env

   opts <- getOptions

   putStrLn "Reading current state..."
   state <- newState token opts
   mstate <- newMVar state

   putStrLn $ "Starting server on: http://localhost:" ++ show (opt_port opts) ++ "/"
   run (opt_port opts) (app mstate)

newState :: Text -> Options -> IO State
newState gitlab_token opts = do
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
         | Just x <- LText.stripPrefix "\"" t
         , Just y <- LText.stripSuffix "\"" x
         = y
         | otherwise
         = t
       !runners = List.nub
                     $ List.sort
                     $ fmap fix_runner
                     $ concat
                     $ fmap (Map.keys . noteTests)
                     $ Map.elems notes

   let gitlab_server = GitLab.defaultGitLabServer
        { GitLab.url   = "https://gitlab.haskell.org"
        , GitLab.token = gitlab_token
        }

   http_manager <- HTTP.newManager HTTP.tlsManagerSettings

   let state = State
        { stateHttpMgr     = http_manager
        , stateGitlab      = gitlab_server
        , stateNotes       = notes
        , stateLastCommits = latests
        , stateTestIds     = test_ids
        , stateRunners     = runners
        , stateOpts        = opts
        }
   putStrLn "Update done."
   return state

menu :: [MenuEntry]
menu =
    [ MenuEntry "/performance" "Performance"
    , MenuEntry "/releases" "Releases"
    , MenuEntry "/repository" "Repository"
    , MenuEntry "/labels" "Labels"
    ]


app :: MVar State -> Application
app mstate request respond = do

  case pathInfo request of
   [] -> do
      let cards =
            [ Card Full do
              "Welcome to GHC dashboard"
            ]
      respond $ htmlResponse $ layout menu cards

   ["labels"] -> do
      state <- readMVar mstate
      p <- getGhcProject state
      card_labels <- cardLabels state p
      card_nolabel <- cardIssuesWithoutLabels state p
      let cards = [ card_nolabel
                  , card_labels
                  ]
      respond $ htmlResponse $ layout menu cards

   ["releases"] -> do
      state <- readMVar mstate
      p <- getGhcProject state
      card_milestones <- cardMilestones state p
      let cards = [ card_milestones
                  ]
      respond $ htmlResponse $ layout menu cards

   ["performance"] -> do
      state <- readMVar mstate
      let cards = [ Card Default do
                      h2_ "Metrics per runner"
                      renderAllRunners state
                  ]
      respond $ htmlResponse $ layout menu cards

   ["repository"] -> do
      state <- readMVar mstate
      p <- getGhcProject state
      let cards = [ Card Default $ a_ [href_ "/commits"] "Recent commits"
                  , cardForks p
                  ]
      respond $ htmlResponse $ layout menu cards

   ["commits"] -> do
      state <- readMVar mstate
      let html = renderCommitList state
      respond $ htmlResponse html

   ["commit",cid] -> do
      state <- readMVar mstate
      let cid' = LText.fromStrict cid
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
      let note = parseNote (LText.fromStrict obj) res
          html = renderNote note
      respond $ htmlResponse html

   ["chart",runner] -> do
      state <- readMVar mstate
      let html = renderCommitChart state (LText.fromStrict runner)
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

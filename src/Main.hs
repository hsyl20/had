{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import System.Process.Typed

import Data.Char
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import Lucid

data State = State
   { stateNotes :: Map ID ID
   }

type ID = ByteString

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    updateNotes
    notes <- readNotes
    let state = State
         { stateNotes = notes
         }
    mstate <- newMVar state
    run 8080 (app mstate)

app :: MVar State -> Application
app mstate request respond = case pathInfo request of
   [] -> do
       last_ids <- lastCommits 100
       state <- readMVar mstate
       let html = renderCommitList state last_ids
       respond $ htmlResponse html

   ["show",obj] -> do
       (_exitCode,res,_err) <- readProcess (shell ("git show " <> show obj))
       respond $ responseLBS
           status200
           [("Content-Type", "text/plain")]
           res

   _ -> respond $ notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

htmlResponse :: Html () -> Response
htmlResponse html = responseLBS status200 [("Content-Type","text/html")] (renderBS html)

--------------------------------------
-- Rendering
--------------------------------------

renderCommitList :: State -> [ID] -> Html ()
renderCommitList state ids = do
   table_ $ do
      let notes = stateNotes state
      tr_ $ do
         th_ "Commit"
         th_ "Perf note"
      forM_ ids $ \cid -> do
         let cid'   = Text.decodeUtf8 cid
         tr_ $ do
            td_ $ do
               a_ [href_ $ "/show/" <> Text.toStrict cid'] $ do
                  toHtmlRaw $ cid'
            td_ $ do
               case Map.lookup cid notes of
                  Just note_id -> do
                     let note_id' = Text.decodeUtf8 note_id
                     a_ [href_ $ "/show/" <> Text.toStrict note_id'] $ do
                        toHtmlRaw $ note_id'
                  Nothing -> "None"

--------------------------------------
-- Git stuff
--------------------------------------

-- | Read performance notes from Git
readNotes :: IO (Map ID ID)
readNotes = do
   (_exitCode,res,_err) <- readProcess "git notes --ref=ci/perf"
   return $ Map.fromList
          $ fmap (\[a,b] -> (b,a)) -- one note per commit?
          $ filter (not . null)
          $ fmap (LBS.split (fromIntegral (ord ' ')))
          $ LBS.split (fromIntegral (ord '\n')) res

-- | Update notes from Gitlab
updateNotes :: IO ()
updateNotes = do
   let cmd = shell ("git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf")
   (_exitCode,_res,_err) <- readProcess cmd
   return ()

-- | Return last N commit IDs from "origin/master"
lastCommits :: Word -> IO [ID]
lastCommits n = do
   let cmd = shell ("git log --pretty=%H origin/master -" <> show n)
   (_exitCode,res,_err) <- readProcess cmd
   return $ LBS.split (fromIntegral (ord '\n')) res

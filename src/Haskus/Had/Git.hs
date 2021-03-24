{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskus.Had.Git where

import System.Process.Typed

import Data.Char
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Lucid

import Haskus.Had.State


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


renderCommitList :: State -> Html ()
renderCommitList state = do
   let ids   = stateLastCommits state
   forM_ ids $ \c -> do
      renderCommit state c
      hr_ [style_ "border: 5px solid green; border-radius: 4px; margin-top:20px; margin-bottom: 20px"]

renderCommit :: State -> (ID,Text) -> Html ()
renderCommit state (cid,summary) = do
   let notes = stateNotes state
   pre_ do
      toHtml summary
   hr_ []
   div_ do
      a_ [href_ $ "/show/" <> Text.toStrict cid] $ "Diff"
      " - "
      case Map.lookup cid notes of
         Just (Note note_id _) -> do
            a_ [href_ $ "/note/" <> Text.toStrict note_id] $ "Perf report"
         Nothing -> "Perf report not available"
      " - "
      commitLink cid "Gitlab"

commitLink :: ID -> Html () -> Html ()
commitLink cid body =
  a_ [ href_ $ "https://gitlab.haskell.org/ghc/ghc/-/commit/" <> Text.toStrict cid
     , target_ "_blank"
     ] body

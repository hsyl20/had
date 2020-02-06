{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import System.Process.Typed

app :: Application
app request respond = case rawPathInfo request of
   "/" -> do
       (_exitCode,branches,_err) <- readProcess "git branch -l"
       respond $ responseLBS
           status200
           [("Content-Type", "text/plain")]
           ("Branches:" <> branches)

   "/perf" -> do
       (_exitCode,perf,_err) <- readProcess "git notes --ref=ci/perf"
       respond $ responseLBS
           status200
           [("Content-Type", "text/plain")]
           ("Perf objects:" <> perf)

   "/perf/fetch" -> do
       let cmd = "git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf"
       (_exitCode,perf,_err) <- readProcess cmd
       respond $ responseLBS
           status200
           [("Content-Type", "text/plain")]
           ("Perf update: " <> perf)


   _ -> respond $ notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app

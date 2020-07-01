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
   { stateNotes :: Map ByteString ByteString
   }

app :: MVar State -> Application
app mstate request respond = case pathInfo request of
   [] -> do
       state <- readMVar mstate
       respond $ htmlResponse (renderCommitList state)

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

renderCommitList :: State -> Html ()
renderCommitList state = do
   table_ $ do
      forM_ (Map.toList $ stateNotes state) $ \(key,value) -> do
         let key'   = Text.decodeUtf8 key
         let value' = Text.decodeUtf8 value
         tr_ $ do
            td_ $ do
               a_ [href_ $ "/show/" <> Text.toStrict key'] $ do
                  toHtmlRaw $ key'
            td_ $ do
               a_ [href_ $ "/show/" <> Text.toStrict value'] $ do
                  toHtmlRaw $ value'

-- | Read performance notes from Git
readNotes :: IO (Map ByteString ByteString)
readNotes = do
   (_exitCode,res,_err) <- readProcess "git notes --ref=ci/perf"
   return $ Map.fromList
          $ fmap (\[a,b] -> (a,b))
          $ filter (not . null)
          $ fmap (LBS.split (fromIntegral (ord ' ')))
          $ LBS.split (fromIntegral (ord '\n')) res

-- | Update notes from Gitlab
updateNotes :: IO ()
updateNotes = do
   let cmd = "git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf"
   (_exitCode,_res,_err) <- readProcess cmd
   return ()

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

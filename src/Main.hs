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
import qualified Data.ByteString.Lazy.Char8 as LBS (readInteger)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Lucid
import Numeric.Natural

data State = State
   { stateNotes :: Map ID ID
   }

type Note   = Map Runner [Test]
type Runner = Text

data Test = Test
   { testName   :: Text
   , testWay    :: Text -- ^ "normal", "optasm", etc.
   , testMetric :: Metric
   , testValue  :: Natural
   }
   deriving (Show)

data Metric
   = Metric !MetricTime !MetricDesc
   deriving (Show,Eq,Ord)

data MetricDesc
   = BytesAlloc
   | PeakMegabytesAlloc
   | MaxBytesUsed
   deriving (Show,Eq,Ord)

data MetricTime
   = CompileTime
   | Runtime
   deriving (Show,Eq,Ord)

type ID = ByteString

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    gitUpdateNotes
    notes <- gitReadNotes
    let state = State
         { stateNotes = notes
         }
    mstate <- newMVar state
    run 8080 (app mstate)

app :: MVar State -> Application
app mstate request respond = case pathInfo request of
   [] -> do
      last_ids <- gitLastCommits 100
      state <- readMVar mstate
      let html = renderCommitList state last_ids
      respond $ htmlResponse html

   ["show",obj] -> do
      res <- gitShowObject obj
      respond $ responseLBS
         status200
         [("Content-Type", "text/plain")]
         res

   ["note",obj] -> do
      res <- gitShowObject obj
      let note = parseNote res
      respond $ responseLBS
         status200
         [("Content-Type", "text/plain")]
         (Text.encodeUtf8 (Text.pack (show note)))

   _ -> respond $ notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

htmlResponse :: Html () -> Response
htmlResponse html = responseLBS status200 [("Content-Type","text/html")] (renderBS html)


parseNote :: ByteString -> Note
parseNote bs = Map.fromListWith (++)
   $ fmap (\[r,n,w,m,v] -> (Text.decodeUtf8 r, [Test
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
                                                         _                          -> error ("Invalid metric: " <> show desc))
                                       _         -> error ("Invalid metric: " <> show m)
                                 , testValue = case LBS.readInteger v of
                                    Nothing -> error ("Invalid metric value: " <> show v)
                                    Just (x,_)  -> fromIntegral x
                                 }]))
                                       
   $ filter (not . null)
   $ fmap (LBS.split (fromIntegral (ord '\t')))
   $ LBS.split (fromIntegral (ord '\n')) bs


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
gitReadNotes :: IO (Map ID ID)
gitReadNotes = do
   (_exitCode,res,_err) <- readProcess "git notes --ref=ci/perf"
   return $ Map.fromList
          $ fmap (\[a,b] -> (b,a)) -- one note per commit?
          $ filter (not . null)
          $ fmap (LBS.split (fromIntegral (ord ' ')))
          $ LBS.split (fromIntegral (ord '\n')) res

-- | Update notes from Gitlab
gitUpdateNotes :: IO ()
gitUpdateNotes = do
   let cmd = shell ("git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf")
   (_exitCode,_res,_err) <- readProcess cmd
   return ()

-- | Return last N commit IDs from "origin/master"
gitLastCommits :: Word -> IO [ID]
gitLastCommits n = do
   let cmd = shell ("git log --pretty=%H origin/master -" <> show n)
   (_exitCode,res,_err) <- readProcess cmd
   return $ LBS.split (fromIntegral (ord '\n')) res

gitShowObject :: Show a => a -> IO ByteString
gitShowObject obj = do
   (_exitCode,res,_err) <- readProcess (shell ("git show " <> show obj))
   return res

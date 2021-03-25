module Haskus.Had.CmdLine where

import Options.Applicative

data Options = Options
   { opt_port     :: !Int
   , opt_ncommits :: !Word
   }

options :: Parser Options
options = Options
  <$> option auto (
        long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8080
     <> help "Use port PORT for the HTTP server")
  <*> option auto (
        short 'n'
     <> metavar "NCOMMITS"
     <> value 1000
     <> help "Consider NCOMMITS latest commits")
      -- don't set this number too high because older note format isn't
      -- supported: metrics for the compiler and for the test programs were
      -- mixed
  

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "GHC dashboard"
     <> header "GHC-DASHBOARD" )

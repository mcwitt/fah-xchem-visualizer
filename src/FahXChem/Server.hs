{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module FahXChem.Server (startServer) where

import qualified Data.Map as Map
import FahXChem.CompoundSeriesAnalysis
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type CompoundAPI = "compounds" :> Get '[JSON] [CompoundAnalysis]

server :: Server CompoundAPI
server = pure []

compoundAPI :: Proxy CompoundAPI
compoundAPI = Proxy

app :: Application
app = serve compoundAPI server

startServer :: Int -> IO ()
startServer port = run port app

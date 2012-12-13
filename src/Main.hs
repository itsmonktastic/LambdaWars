module Main where

import Core
import Web

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

serverLoop :: Broadcaster -> IO ()
serverLoop broadcast = forever $ do
	-- XXX: just stubbed out atm, wait 1 second then broadcast an empty world
	threadDelay 1000000
	broadcast $ World [] []

main = do
	withServerDo $ \broadcast -> do
		putStrLn "Web server up and running"
		serverLoop broadcast

module Web (withServerDo, Broadcaster) where

import Snap.Http.Server
import Snap.Core
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newChan, Chan, writeChan, readChan)
import Data.IORef
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)

import Core (World)

type Broadcaster = World -> IO ()

getLatestRender :: IORef (Maybe B.ByteString) -> Snap ()
getLatestRender latestRender = do
	maybeRender <- liftIO $ readIORef latestRender
	case maybeRender of
		Nothing -> writeBS "No world to render!"
		Just render -> writeBS render

site :: IORef (Maybe B.ByteString) -> Snap ()
site latestRender =
	ifTop (writeBS "hello world") <|>
	route [
		("world.svg", getLatestRender latestRender)
	]

renderer :: IORef (Maybe B.ByteString) -> Chan World -> IO ()
renderer latestRender broadcastChan = forever $ do
	world <- readChan broadcastChan
	writeIORef latestRender $ Just (B.pack $ show $ world)

withServerDo :: (Broadcaster -> IO ()) -> IO ()
withServerDo actionWithBroadcaster = do
	-- snap logs to stderr and says "THIS IS BAD" 
	-- if it can't create log files in ./log
	let createParents = False in createDirectoryIfMissing createParents "log"
	latestRender <- newIORef Nothing
	broadcastChan <- newChan :: IO (Chan World)
	forkIO $ quickHttpServe (site latestRender)
	forkIO $ renderer latestRender broadcastChan
	actionWithBroadcaster (writeChan broadcastChan)

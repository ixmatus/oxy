module Network.HTTP.Oxy.Proxy where

import           Network.Fancy
import           OxyTypes
import           System.Log.Logger
import           System.Process

proxyServer :: IO (Settings) -> IO ()
proxyServer settings = do
    
    sets <- settings
    
    infoM "Console" "Setting up!"
    
    -- bind to a specific IP/Address or, if bindsame is True, bind to
    -- all interfaces and server from the specified port
    _ <- streamServer (ServerSpec ((hostBind (bindsame sets) (host sets)) $ port sets) ReverseName Threaded False 200) (\_h sa -> debugM "Console" (show sa))
    
    -- using this instead of network-fancy's "sleepForever" as it uses
    -- threadDelay which maxes out the CPU to 100% on Mac OS X
    sleepFrvr

hostBind :: Bool -> HostName -> Port -> Address
hostBind True _   = (IP "")
hostBind False ip = (IP ip)

sleepFrvr :: IO ()
sleepFrvr = system "sleep 1" >> sleepFrvr

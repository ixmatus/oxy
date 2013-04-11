{-# LANGUAGE DeriveDataTypeable #-}

module OxyTypes where

import           Network.Fancy (HostName)

import           System.Console.CmdArgs (Typeable, Data)
import           System.Log.Logger

type Port = Int
type Timeout = Int
type LogLevel = Priority

-- GADT for cmdargs
data OxyArgs = Args
    {
        config    :: String,
        daemonize :: Bool
    } deriving (Data,Typeable,Show,Eq)

-- GADT for post-config parsed application settings
data Settings = Settings
    {
        daemon    :: Bool,
        port      :: Port,
        host      :: HostName,
        bindsame  :: Bool,
        timeout   :: Timeout,
        cookies   :: Bool,
        viaheader :: Bool,
        pidfile   :: String,
        loglevel  :: LogLevel,
        logfile   :: String
    } deriving (Show, Eq)

module Main where

import           Network.HTTP.Oxy.Proxy
import           OxyTypes
import           System.Console.CmdArgs
import           System.Log.Handler.Color
import           System.Log.Logger
import           System.Exit
import           Data.ConfigFile
import           Control.Monad.Error

oxyargs :: OxyArgs
oxyargs = Args
    {
        config    = "/usr/local/etc/oxy.ini" &= opt "/usr/local/etc/oxy.ini" &= help "Specify config file to use",
        daemonize = def &= name "d" &= help "Run server in the background"
    } &=
    verbosity &=
    help    "Lightweight forwarding proxy server" &=
    helpArg [name "h"] &=
    summary "Oxy v0.0.1, (C) Parnell Springmeyer" &=
    details ["Oxy is a simple to configure and light-weight forwarding proxy server.",
             "",
             "Default configuration file is \"oxy.ini\", use --config=[File] to specify a custom one."]

main :: IO ()
main = cmdArgs oxyargs >>= proxyServer . parseConfig

parseConfig :: OxyArgs -> IO (Settings)
parseConfig argvs = do
    updateGlobalLogger "Console" (setLevel INFO)

    -- activate color logging
    updateGlobalLogger rootLoggerName (addHandler colorHandler)

    infoM "Console" $ "Initializing Oxy with " ++ (config argvs)
    
    -- parse the config file
    cnf <- runErrorT $ do
        cp          <- join $ liftIO $ readfile emptyCP "oxy.ini"
        portv       <- get cp "NETWORK" "port"
        hostv       <- get cp "NETWORK" "host"
        bindsamev   <- get cp "NETWORK" "bindsame"
        timeoutv    <- get cp "NETWORK" "timeout"
        cookiesv    <- get cp "NETWORK" "cookies"
        viaheaderv  <- get cp "NETWORK" "viaheader"
        pidfilev    <- get cp "NETWORK" "pidfile"
        loglevelv   <- get cp "NETWORK" "loglevel"
        logfilev    <- get cp "NETWORK" "logfile"
        
        -- build and return a settings record
        return Settings {
                    daemon    = daemonize argvs,
                    port      = portv,
                    host      = hostv,
                    bindsame  = bindsamev,
                    timeout   = timeoutv,
                    cookies   = cookiesv,
                    viaheader = viaheaderv,
                    pidfile   = pidfilev,
                    loglevel  = loglevelv,
                    logfile   = logfilev
                }
    
    handleConfig cnf

handleConfig :: Either (CPErrorData, String) Settings -> IO (Settings)
handleConfig (Left err) = do
    -- log the error and exit the program
    criticalM "Console" $ show err
    criticalM "Console" "exiting..."
    exitWith (ExitFailure 1)
handleConfig (Right conf) = do
    infoM "Console" "Configuration parsed"
    infoM "Console" $ "Setting loglevel to " ++ (show $ loglevel conf)
    
    updateGlobalLogger "Console" (setLevel $ loglevel conf)
    
    return conf

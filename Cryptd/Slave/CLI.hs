{-# LANGUAGE DeriveDataTypeable #-}
-- | Command line processing for the slave daemon.
module Cryptd.Slave.CLI (run, SlaveSettings(..)) where

import System.Console.CmdArgs.Implicit

import qualified Cryptd.Lib.ConfigEmbed as Conf

-- | Settings for running the slave daemon.
data SlaveSettings = SlaveSettings
    { listenAddress :: String
    -- ^ Host/IP to listen on for the external backend
    , port :: Integer
    -- ^ Port to listen on for the external backend
    , masterHost :: String
    -- ^ Host/IP of the master server
    , masterPort :: Integer
    -- ^ Port of the master server
    , url :: String
    -- ^ URL for the external backend on incoming requests
    , foreground :: Bool
    -- ^ Run in foreground
    , instanceId :: Maybe String
    -- ^ A value distinguishing the slave from other slaves with the same ID
    }
    deriving (Show, Data, Typeable)

-- | Return 'SlaveSettings' annotated for "System.Console.CmdArgs.Implicit".
slave :: SlaveSettings
slave = SlaveSettings
    { listenAddress = "127.0.0.1"
                   &= typ "ADDRESS"
                   &= help "Local listen address"
    , port          = 16661
                   &= typ "PORT"
                   &= help "Local port to listen on"
    , masterHost    = Conf.masterHost
                   &= typ "ADDRESS"
                   &= help "Hostname/IP of the master server"
    , masterPort    = 26662
                   &= typ "PORT"
                   &= help "Port of the master server"
    , url           = Conf.url
                   &= typ "URL"
                   &= help ("URL to the local API backend" ++ defurl)
    , foreground    = False
                   &= help "Run cryptd in the foreground"
    , instanceId    = Nothing
                   &= typ "INSTANCE_IDENTIFIER"
                   &= opt ""
                   &= argPos 0
    }
    &= program "cryptd-slave"
    &= summary "MoonID encryption daemon - Slave server"
    &= details ["(C) 2012 RedMoon Studios GmbH & Co KG"]
  where
    defurl = " (Default: " ++ Conf.url ++ ")"

-- | Parse commandline options.
run :: IO SlaveSettings
run = cmdArgs slave

import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TChan
import Network.Wai (Application)
import Network.Wai.Handler.Warp ( runSettings
                                , defaultSettings
                                , Settings(..)
                                , HostPreference(Host)
                                )
import qualified Data.ByteString.Lazy.Char8 as LB

import Cryptd.Lib.TLS
import Cryptd.Lib.Tunnel
import Cryptd.Lib.HTTPSerial (consumeRequest)
import Cryptd.Lib.Callbacks (requestLoop)
import Cryptd.Lib.ConfigEmbed (publicX509, privateKey, secret)
import Cryptd.Lib.Daemonize (daemonize)

import Cryptd.Slave.CLI

-- | The 'Application' handling requests from the external backend.
app :: TunnelState -> Application
app ts req = liftIO $ do
    fullReq <- consumeRequest req
    atomically $ writeTChan (outChannel ts) (ChannelRequest fullReq)
    atomically $ do
        val <- readTChan (inChannel ts)
        case val of
             ChannelResponse r -> return r
             _ -> retry

-- | Send patched in secret to 'TunnelHandle' and return True on success.
sendSecret :: TunnelHandle -> IO Bool
sendSecret h = sendData h (LB.pack secret) >> return True

-- | Send the 'instanceId' to 'TunnelHandle' and return True on success.
sendInstance :: Maybe String -> TunnelHandle -> IO Bool
sendInstance i h =
    sendData h (maybeToRaw i) >> return True
  where
    maybeToRaw (Just "") = maybeToRaw Nothing
    maybeToRaw (Just v)  = LB.cons' '\001' (LB.pack v)
    maybeToRaw Nothing   = LB.singleton '\000'

-- | Combine 'sendSecret' and 'sendInstance'
sendInit :: Maybe String -> TunnelHandle -> TunnelState -> IO Bool
sendInit i h _ = sendSecret h >> sendInstance i h >> return True

-- | Handle parsed commandline arguments from SlaveSettings and if everything is
-- fine, run the slave daemon.
dispatch :: SlaveSettings -> IO ()
dispatch ss = maybeDaemonize (foreground ss) $ do
    (state, handler) <- makeHandler callbacks
    let certs = (publicX509, privateKey)
    _ <- runTLSClient $ makeSettings connectTo connectPort certs handler
    runSettings tlsSettings (app state)
    return ()
  where
    maybeDaemonize True = id
    maybeDaemonize False = daemonize "cryptd-slave"

    connectTo = masterHost ss
    connectPort = fromIntegral $ masterPort ss
    tlsSettings = defaultSettings
        { settingsHost = Host $ listenAddress ss
        , settingsPort = fromInteger $ port ss
        }

    callbacks = noCallbacks
        { onConnect = sendInit (instanceId ss)
        , onLoop = requestLoop $ url ss
        }

main :: IO ()
main = dispatch =<< run

module Socket (module Socket) where

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client.Internal
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

newManager :: FilePath -> IO Client.Manager
newManager socketPath =
    Client.newManager
        Client.defaultManagerSettings
            { Client.managerRawConnection = pure mkUnixSocket
            }
  where
    mkUnixSocket :: Maybe S.HostAddress -> String -> Int -> IO Client.Internal.Connection
    mkUnixSocket _ _ _ = do
        s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
        S.connect s (S.SockAddrUnix socketPath)
        Client.Internal.makeConnection
            (SBS.recv s 8096)
            (SBS.sendAll s)
            (S.close s)
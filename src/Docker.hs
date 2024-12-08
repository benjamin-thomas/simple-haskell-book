{-# LANGUAGE OverloadedStrings #-}

module Docker (module Docker) where

import Data.Text (Text)
import qualified Network.HTTP.Simple as HTTP

import Debug.Pretty.Simple (pTraceIO)

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Network.HTTP.Conduit (Request)
import qualified Socket

import Control.Exception (Exception)
import Control.Monad (void)
import Data.Aeson (FromJSON, Value (Object), (.:))
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (Manager)

newtype Image = Image Text
    deriving (Show, Eq)

imageToText :: Image -> Text
imageToText (Image image_) = image_

data CreateContainerOptions = CreateContainerOptions
    { image :: Image
    , script :: Text
    }

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Show, Eq)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode exitCode) = exitCode

newtype ContainerId = ContainerId Text
    deriving (Show, Eq)

containerIdToText :: ContainerId -> Text
containerIdToText = coerce

data Service = Service
    { createContainer :: CreateContainerOptions -> IO (Either CreateContainerError ContainerId)
    , startContainer :: ContainerId -> IO ()
    , containerStatus :: ContainerId -> IO (Either ContainerStatusResponseError ContainerStatus)
    }

createService :: IO Service
createService = do
    manager <- initManager

    let mkRequest :: ByteString -> Request
        mkRequest path =
            HTTP.defaultRequest
                & HTTP.setRequestManager manager
                & HTTP.setRequestPath (apiVersion </> path)
    pure $
        Service
            { createContainer = createContainer_ mkRequest
            , startContainer = startContainer_ mkRequest
            , containerStatus = containerStatus_ mkRequest
            }

newtype DockerParseException = DockerParseException Text
    deriving (Show)

instance Exception DockerParseException

newtype CreateContainerError
    = CreateContainerParseResponseFailed Text
    deriving (Show)

instance FromJSON ContainerId where
    parseJSON (Object obj) = ContainerId <$> (obj .: "Id")
    parseJSON _ = fail "not an object"

data ContainerStatus
    = ContainerRunning
    | ContainerExited ContainerExitCode
    | ContainerUnknownStatus Text

initManager :: IO Manager
initManager = Socket.newManager "/var/run/docker.sock"

apiVersion :: ByteString
apiVersion = "/v1.47"

{- | Combine two paths, ensuring no double slashes can occur.

>>> "a" </> "b"
"a/b"

>>> "a/" </> "b"
"a/b"

>>> "a" </> "/b"
"a/b"

>>> "a/" </> "/b"
"a/b"
-}
(</>) :: ByteString -> ByteString -> ByteString
(</>) a b
    | BS.isSuffixOf "/" a && BS.isPrefixOf "/" b = a <> BS.drop 1 b
    | BS.isSuffixOf "/" a || BS.isPrefixOf "/" b = a <> b
    | otherwise = a <> "/" <> b

{-

To verify the latest API version available on the local docker daemon, run:

curl -s --unix-socket /var/run/docker.sock http://localhost/version | jq .ApiVersion

Prior versions seem to be supported as well

---

Inspect the data via the REPL this way:

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple
> :m +Docker +Network.HTTP.Client
> :set -XOverloadedStrings

res <- createContainer_ $ CreateContainerOptions $ Image "hello-world"
 -}
createContainer_ :: (ByteString -> Request) -> CreateContainerOptions -> IO (Either CreateContainerError ContainerId)
createContainer_ mkRequest options = do
    let script' = script options
    TIO.putStrLn ("Creating container with script: " <> script')
    let body :: Aeson.Value
        body =
            Aeson.object
                [ ("Image", Aeson.String $ imageToText $ image options)
                , ("Tty", Aeson.Bool True)
                , ("Labels", Aeson.object [("quad", "")])
                , ("Entrypoint", Aeson.toJSON entrypoint)
                , -- , ("Cmd", "echo hello")
                  ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> script'])
                , ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh")
                ]
          where
            entrypoint :: [Aeson.Value]
            entrypoint =
                Aeson.String
                    <$> [ "/bin/sh"
                        , "-c"
                        ]

    let req :: Request
        req =
            mkRequest "/containers/create"
                & HTTP.setRequestMethod "POST"
                & HTTP.setRequestBodyJSON body

    let
        parseResponse :: ByteString -> Either String ContainerId
        parseResponse = Aeson.eitherDecodeStrict'

    res <- HTTP.httpBS req
    let respBody = HTTP.getResponseBody res
    let x = Aeson.encode body
    pTraceIO ("==> Sent:" <> show x)
    pTraceIO ("==> Got:" <> show respBody)

    TIO.putStrLn ("Response body: " <> decodeUtf8 respBody)
    return $
        first -- first is map error
            (CreateContainerParseResponseFailed . T.pack)
            (parseResponse (HTTP.getResponseBody res))

{-

To test in GHCi:

> Right container <- createContainer_ $ CreateContainerOptions $ Image "alpine"
> startContainer_ container
-}
startContainer_ :: (ByteString -> Request) -> ContainerId -> IO ()
startContainer_ mkRequest (ContainerId containerId) = do
    let req =
            mkRequest ("/containers/" <> encodeUtf8 containerId <> "/start")
                & HTTP.setRequestMethod "POST"

    void $ HTTP.httpBS req

newtype ContainerStatusResponseError
    = ContainerStatusResponseParseJsonError Text
    deriving (Show)

{-

curl -s --unix-socket /var/run/docker.sock http://localhost/containers/a9998fab3485/json | jq .State

{
  "Status": "running",
  "Running": true,
  "Paused": false,
  "Restarting": false,
  "OOMKilled": false,
  "Dead": false,
  "Pid": 1911,
  "ExitCode": 0,
  "Error": "",
  "StartedAt": "2024-11-24T19:03:50.508653295Z",
  "FinishedAt": "2024-11-24T19:03:21.059045849Z"
}

 -}
containerStatus_ :: (ByteString -> Request) -> ContainerId -> IO (Either ContainerStatusResponseError ContainerStatus)
containerStatus_ mkRequest containerId = do
    let
        parser :: Value -> Aeson.Parser ContainerStatus
        parser = Aeson.withObject "container-inspect" $ \o -> do
            state <- o .: "State"
            status <- state .: "Status"
            case status of
                "running" -> pure ContainerRunning
                "exited" -> do
                    exitCode <- state .: "ExitCode"
                    pure $ ContainerExited (ContainerExitCode exitCode)
                _ -> pure $ ContainerUnknownStatus status
    let req = mkRequest $ "/containers/" <> encodeUtf8 (containerIdToText containerId) <> "/json"
    res <- HTTP.httpBS req
    let
        parseResponse :: ByteString -> Either String ContainerStatus
        parseResponse bs = do
            value <- Aeson.eitherDecodeStrict' bs
            Aeson.parseEither parser value
    return $
        first -- first is map error
            (ContainerStatusResponseParseJsonError . T.pack)
            (parseResponse (HTTP.getResponseBody res))
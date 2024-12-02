{-# LANGUAGE OverloadedStrings #-}

module Docker (module Docker) where

import Data.Text (Text)
import qualified Network.HTTP.Simple as HTTP

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Network.HTTP.Conduit (Request)
import qualified Socket

import Control.Exception (Exception)
import Control.Monad (void)
import Data.Aeson (FromJSON, Value (Object), (.:))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager)

newtype Image = Image Text
    deriving (Show, Eq)

imageToText :: Image -> Text
imageToText (Image image_) = image_

newtype CreateContainerOptions = CreateContainerOptions
    { image :: Image
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
    }

createService :: Service
createService =
    Service
        { createContainer = createContainer_
        , startContainer = startContainer_
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
createContainer_ :: CreateContainerOptions -> IO (Either CreateContainerError ContainerId)
createContainer_ options = do
    manager <- initManager

    let body :: Aeson.Value
        body =
            Aeson.object
                [ ("Image", Aeson.String $ imageToText $ image options)
                , ("Tty", Aeson.Bool True)
                , ("Labels", Aeson.object [("quad", "")])
                , ("Cmd", "echo hello")
                , ("Entrypoint", Aeson.toJSON entrypoint)
                ]
          where
            entrypoint :: [Aeson.Value]
            entrypoint =
                Aeson.String
                    <$> [ "/bin/sh"
                        , "-c"
                        , "echo hello"
                        ]

    let req :: Request
        req =
            HTTP.setRequestManager manager
                . HTTP.setRequestPath (apiVersion </> "/containers/create")
                . HTTP.setRequestMethod "POST"
                . HTTP.setRequestBodyJSON body
                $ HTTP.defaultRequest

    let
        parseResponse :: ByteString -> Either String ContainerId
        parseResponse = Aeson.eitherDecodeStrict'

    res <- HTTP.httpBS req
    return $
        first -- first is map error
            (CreateContainerParseResponseFailed . T.pack)
            (parseResponse (HTTP.getResponseBody res))

{-

To test in GHCi:

> Right container <- createContainer_ $ CreateContainerOptions $ Image "alpine"
> startContainer_ container
-}
startContainer_ :: ContainerId -> IO ()
startContainer_ (ContainerId containerId) = do
    manger <- initManager
    let req =
            HTTP.setRequestManager manger
                . HTTP.setRequestPath (apiVersion </> "/containers" </> encodeUtf8 containerId </> "/start")
                . HTTP.setRequestMethod "POST"
                $ HTTP.defaultRequest
    void $ HTTP.httpBS req

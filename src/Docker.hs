{-# LANGUAGE OverloadedStrings #-}

module Docker (module Docker) where

import Data.Text (Text)
import qualified Network.HTTP.Simple as HTTP

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Network.HTTP.Conduit (Request)
import qualified Socket

import Control.Exception (Exception, throwIO)
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.Text as T

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

newtype DockerParseException = DockerParseException Text
    deriving (Show)

instance Exception DockerParseException

newtype CreateContainerError
    = CreateContainerParseResponseFailed Text
    deriving (Show)

-- instance FromJSON ContainerId where
--     parseJSON = \v -> do
--         cId <- parseJSON v
--         pure $ ContainerId cId

{-

To verify the latest API version available on the local docker daemon, run:

curl -s --unix-socket /var/run/docker.sock http://localhost/version | jq .ApiVersion

Prior versions seem to be supported as well

---

Inspect the data via the REPL this way:

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple
> :m +Docker +Network.HTTP.Client
> :set -XOverloadedStrings

res <- createContainer $ CreateContainerOptions $ Image "hello-world"
responseBody res
 -}
createContainerExn :: CreateContainerOptions -> IO (Either CreateContainerError ContainerId)
createContainerExn options = do
    manager <- Socket.newManager "/var/run/docker.sock"

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
                . HTTP.setRequestPath "/v1.47/containers/create"
                . HTTP.setRequestMethod "POST"
                . HTTP.setRequestBodyJSON body
                $ HTTP.defaultRequest

    let container :: Aeson.Object -> Parser ContainerId
        container obj = ContainerId <$> (obj .: "Id")

    let
        parseResponse :: ByteString -> Either String ContainerId
        parseResponse resp =
            Aeson.eitherDecodeStrict resp >>= \obj ->
                parseEither container obj

    res <- HTTP.httpBS req
    return $
        first -- first is map error
            (CreateContainerParseResponseFailed . T.pack)
            (parseResponse (HTTP.getResponseBody res))
{-# LANGUAGE OverloadedStrings #-}

module Docker (module Docker) where

import Data.Text (Text)
import qualified Network.HTTP.Simple as HTTP

import qualified Data.Aeson as Aeson
import Debug.Trace (traceIO)
import Network.HTTP.Conduit
import qualified Socket

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
-- createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
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

    res <- HTTP.httpBS req
    traceIO (show res)
    return res
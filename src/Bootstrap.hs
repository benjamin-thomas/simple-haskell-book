{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bootstrap
    ( main
    , createAlpine
    , firstBuild
    , transition
    , initFirstBuild
    , launchFirstBuild
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Core (Build (..), BuildStatus (Done, Failed, Ready, Running), Step (..))
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Coerce (coerce)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Pretty.Simple
import Docker
import System.Process.Typed (proc, readProcess_, runProcess_)
import Text.Pretty.Simple
import Prelude hiding (fail)

deleteQuadContainersExn :: IO ()
deleteQuadContainersExn = do
    -- FIXME: I tried and failed to stream in an xargs fashion. Give it another go later.
    --
    -- As an exercise, I'm not using the simpler `readProcessStdout` as suggested in the book.
    -- It opens up the door to shell injection if one isn't careful.
    --
    -- Observe stdout with:
    -- cabal test --test-show-details=direct
    putStrLn "==> Deleting quad containers"
    (out, _err) <- readProcess_ $ proc "docker" ["ps", "-aq", "--filter", "label=quad"]
    let containerIds = C8L.unpack <$> C8L.lines out
    unless (null containerIds) $ do
        putStrLn "==> Deleting containers!"
        print containerIds
        runProcess_ $ proc "docker" ("rm" : "-f" : containerIds)

alpine :: Docker.CreateContainerOptions
alpine =
    Docker.CreateContainerOptions
        "alpine"
        "echo 'hello world'"

{-
Right containerId <- createAlpine
 -}
createAlpine :: IO (Either CreateContainerError ContainerId)
createAlpine = do
    svc <- Docker.createService
    Docker.createContainer svc alpine

firstBuild :: Build
firstBuild =
    Build
        { status = Ready
        , todo =
            [ Step "First step" "alpine" "echo 'hello world1'"
            , Step "Second step" "alpine" "echo 'hello world2'"
            ]
        , done = []
        }

launchFirstBuild :: IO Build
launchFirstBuild = do
    deleteQuadContainersExn
    svc <- Docker.createService
    final <- runUntilSettled svc firstBuild
    pTraceIO $ "Build settled:" <> show final
    pure final

millisec :: Int
millisec = 1000

quote :: (Semigroup a, IsString a) => a -> a
quote s = "'" <> s <> "'"

{-

initFirstBuild

 -}
runUntilSettled :: Docker.Service -> Build -> IO Build
runUntilSettled svc build = do
    case build.status of
        Done -> do
            TIO.putStrLn "[SETTLE] build is done"
            pure build
        Failed _ -> do
            TIO.putStrLn "[SETTLE] build failed"
            pure build
        Ready -> do
            TIO.putStrLn "[SETTLE] ready to next"
            next <- transition svc build
            runUntilSettled svc next
        Running _ -> do
            TIO.putStrLn "[SETTLE] running to next"
            next <- transition svc build
            threadDelay $ 100 * millisec
            runUntilSettled svc next

{-

(svc, wip1) <- initFirstBuild
wip2 <- transition svc wip1
wip3 <- transition svc wip2
wip4 <- transition svc wip3
wip5 <- transition svc wip4
wip6 <- transition svc wip5

 -}
-- initFirstBuild :: IO Build
initFirstBuild :: IO (Service, Build)
initFirstBuild = do
    svc <- Docker.createService
    wip <- transition svc firstBuild
    pure (svc, wip)

transition :: Docker.Service -> Build -> IO Build
transition svc build = do
    -- pTraceIO $ show build
    case build.status of
        Done -> do
            TIO.putStrLn "Build is already done! (NOOP)"
            pure build
        Failed s -> do
            TIO.putStrLn
                ( "Build step "
                    <> quote s.title
                    <> " has failed, not advancing further!"
                )
            pure build
        Ready -> do
            TIO.putStrLn "No container running, running the next build step..."
            case build.todo of
                [] -> do
                    putStrLn "No more steps to run, marking the build as done!"
                    pure build{status = Done}
                x : xs -> do
                    TIO.putStrLn $ "Running step: " <> x.title
                    let opts :: CreateContainerOptions
                        opts = Docker.CreateContainerOptions x.image x.command
                    createResult <- Docker.createContainer svc opts

                    case createResult of
                        Left err -> do
                            TIO.putStrLn $ "⚠ Failed to create container: " <> T.pack (show err)
                            pure $ build{status = Failed x}
                        Right containerId -> do
                            TIO.putStrLn $ "Container created! Starting it up... (id=" <> coerce containerId <> ")"
                            Docker.startContainer svc containerId
                            TIO.putStrLn $ "Container started! Marking as running. (id=" <> coerce containerId <> ")"
                            pure
                                build
                                    { status = Running (containerId, x)
                                    , todo = xs
                                    }
        Running (containerId, step) -> do
            TIO.putStrLn "Container is running, checking status for failure..."
            -- TODO: actually check the container status
            containerStatus <- Docker.getContainerStatus svc containerId
            case containerStatus of
                Left err -> do
                    TIO.putStrLn $ "⚠ Failed to get container status: " <> T.pack (show err)
                    TIO.putStrLn "⚠ Marking build as failed"
                    pure $
                        build
                            { status = Failed step
                            }
                Right Docker.ContainerRunning -> do
                    TIO.putStrLn "Container is still running, NOOP (check back later)"
                    pure build
                Right (Docker.ContainerExited exitCode) -> do
                    TIO.putStrLn $ "Container exited with code: " <> T.pack (show exitCode)
                    TIO.putStrLn $ "Marking the step " <> quote step.title <> " as done"
                    pure $
                        build
                            { done = step : build.done
                            , status = Ready
                            }

main :: IO ()
main = putStrLn "Booting up..."
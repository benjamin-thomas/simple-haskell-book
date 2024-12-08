{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core (module Core) where

import Control.Exception (throwIO)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Pretty.Simple
import Docker (CreateContainerOptions (..), Service (containerStatus))
import qualified Docker

newtype StepName = StepName Text
    deriving
        ( Show
        , Eq
        , Ord -- enables usage as a Map key
        )

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

data Step = Step
    { stepName :: StepName
    , stepImage :: Docker.Image
    , stepCommands :: NonEmpty Text
    }
    deriving (Show, Eq)

data Pipeline
    = Pipeline
    { pipelineSteps :: NonEmpty Step
    , pipelineState :: BuildState
    }
    deriving (Show, Eq)

data BuildResult
    = BuildSuccess
    | BuildFailed
    deriving (Show, Eq)

data BuildRunningState = BuildRunningState
    { currentStepName :: StepName
    , containerId :: Docker.ContainerId
    }
    deriving (Show, Eq)

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished BuildResult
    deriving (Show, Eq)

data StepResult
    = StepFailed Docker.ContainerExitCode
    | StepSucceeded
    deriving (Show, Eq)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exitCode = case Docker.exitCodeToInt exitCode of
    0 -> StepSucceeded
    _ -> StepFailed exitCode

data Build = Build
    { buildPipeline :: Pipeline
    , buildState :: BuildState
    , buildCompletedSteps :: Map StepName StepResult -- I think this data really belongs to the `BuildRunning` variant
    }
    deriving (Show)

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
    if anyPriorStepsFailed
        then
            Left BuildFailed
        else
            maybe (Left BuildSuccess) Right nextStep
  where
    anyPriorStepsFailed :: Bool
    anyPriorStepsFailed =
        any
            hasFailed
            (buildCompletedSteps build)
      where
        hasFailed :: StepResult -> Bool
        hasFailed = \case StepFailed _ -> True; _ -> False

    nextStep :: Maybe Step
    nextStep =
        List.find
            (not . isCompleted)
            (NE.toList $ pipelineSteps $ buildPipeline build)
      where
        isCompleted :: Step -> Bool
        isCompleted step =
            Map.member
                (stepName step)
                (buildCompletedSteps build)

progress :: Docker.Service -> Build -> IO Build
progress dockerService build = do
    pTraceIO $ show build
    case buildState build of
        BuildReady ->
            case buildHasNextStep build of
                Left result -> do
                    TIO.putStrLn "Transitioning to `BuildFinished` state"
                    pure $
                        build
                            { buildState = BuildFinished result
                            }
                Right step -> do
                    TIO.putStrLn "Attempt starting container..."
                    let script' = T.unlines $ "set -ex" : NE.toList (stepCommands step)
                    let image' = stepImage step
                    let createOptions =
                            Docker.CreateContainerOptions
                                { image = image'
                                , script = script'
                                }
                    containerId' <-
                        either
                            -- TODO: decide how to handle errors
                            ( \err -> do
                                TIO.putStrLn $ "/!\\ Failed to create container: " <> T.pack (show err)
                                throwIO $ userError "Failed to create container: "
                            )
                            pure
                            =<< Docker.createContainer dockerService createOptions
                    Docker.startContainer dockerService containerId'
                    TIO.putStrLn "... container started! Transitioning to `BuildRunning` state"
                    pure $
                        build
                            { buildState =
                                BuildRunning $
                                    BuildRunningState
                                        { currentStepName = stepName step
                                        , containerId = containerId'
                                        }
                            }
        BuildRunning state -> do
            TIO.putStrLn "Container is running, checking status for failure..."
            status <- containerStatus dockerService (containerId state)
            case status of
                Right Docker.ContainerRunning -> do
                    TIO.putStrLn "Container is still running, NOOP"
                    pure build
                Right (Docker.ContainerExited exitCode) -> do
                    let stepResult = exitCodeToStepResult exitCode
                    pure
                        build
                            { buildState =
                                case stepResult of
                                    StepFailed _ -> BuildFinished BuildFailed
                                    StepSucceeded -> BuildFinished BuildSuccess
                            , buildCompletedSteps =
                                Map.insert
                                    (currentStepName state)
                                    stepResult
                                    (buildCompletedSteps build)
                            }
                Right (Docker.ContainerUnknownStatus status') -> do
                    TIO.putStrLn $ "Unexpected container status: " <> status'
                    pure $
                        build{buildState = BuildFinished BuildFailed}
                Left err -> do
                    TIO.putStrLn $ "Unexpected error: " <> T.pack (show err)
                    pure $ build{buildState = BuildFinished BuildFailed}
        BuildFinished _ -> do
            pure build

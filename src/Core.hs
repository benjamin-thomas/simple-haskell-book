{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core (module Core) where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO

newtype StepName = StepName Text
    deriving
        ( Show
        , Eq
        , Ord -- enables usage as a Map key
        )

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

newtype Image = Image Text
    deriving (Show, Eq)

imageToText :: Image -> Text
imageToText (Image image_) = image_

data Step = Step
    { stepName :: StepName
    , stepCommands :: NonEmpty Text
    , stepImage :: Image
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

newtype BuildRunningState = BuildRunningState
    { currentStepName :: StepName
    }
    deriving (Show, Eq)

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished BuildResult
    deriving (Show, Eq)

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Show, Eq)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode exitCode) = exitCode

data StepResult
    = StepFailed ContainerExitCode
    | StepSucceeded
    deriving (Show, Eq)

extCodeToStepResult :: ContainerExitCode -> StepResult
extCodeToStepResult exitCode = case exitCodeToInt exitCode of
    0 -> StepSucceeded
    _ -> StepFailed exitCode

data Build = Build
    { buildPipeline :: Pipeline
    , buildState :: BuildState
    , buildCompletedSteps :: Map StepName StepResult -- I think this data really belongs to the `BuildRunning` variant
    }

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

progress :: Build -> IO Build
progress build = case buildState build of
    BuildReady ->
        case buildHasNextStep build of
            Left result -> do
                TIO.putStrLn "Transitioning to `BuildFinished` state"
                pure $ build{buildState = BuildFinished result}
            Right step -> do
                TIO.putStrLn "Attempt starting container..."
                TIO.putStrLn "... container started! Transitioning to `BuildRunning` state"
                pure $
                    build
                        { buildState =
                            BuildRunning $
                                BuildRunningState{currentStepName = stepName step}
                        }
    BuildRunning state -> do
        TIO.putStrLn "Waiting for container to stop..."
        let result = extCodeToStepResult $ ContainerExitCode 0
        TIO.putStrLn "... container stopped! Exit code was: 0. Transitioning to `BuildFinished` state"
        pure
            build
                { buildState = BuildReady
                , buildCompletedSteps =
                    Map.insert
                        (currentStepName state)
                        result
                        (buildCompletedSteps build)
                }
    BuildFinished _ ->
        pure build
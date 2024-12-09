{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core (module Core) where

import Data.Text (Text)
import Debug.Pretty.Simple
import qualified Docker

data BuildStatus
    = Ready
    | Failed Step
    | Running (Docker.ContainerId, Step)
    | Done
    deriving (Show)

data Build = Build
    { status :: BuildStatus
    , todo :: [Step]
    , done :: [Step]
    }
    deriving (Show)

data Step = Step
    { title :: Text
    , image :: Text
    , command :: Text
    }
    deriving (Show, Eq)

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished Int

data BuildRunningState = BuildRunningState
    { stepName :: Text
    , containerId :: Docker.ContainerId
    }

executeBuild :: Docker.Service -> Build -> IO Build
executeBuild dockerService build = do
    pTraceIO $ show build
    pure build

-- buildHasNextStep :: Build -> Either BuildResult Step
-- buildHasNextStep build =
--     if anyPriorStepsFailed
--         then
--             Left BuildFailed
--         else
--             maybe (Left BuildSuccess) Right nextStep
--   where
--     anyPriorStepsFailed :: Bool
--     anyPriorStepsFailed =
--         any
--             hasFailed
--             (buildCompletedSteps build)
--       where
--         hasFailed :: StepResult -> Bool
--         hasFailed = \case StepFailed _ -> True; _ -> False

--     nextStep :: Maybe Step
--     nextStep =
--         List.find
--             (not . isCompleted)
--             (NE.toList $ pipelineSteps $ buildPipeline build)
--       where
--         isCompleted :: Step -> Bool
--         isCompleted step =
--             Map.member
--                 (stepName step)
--                 (buildCompletedSteps build)

-- progress :: Docker.Service -> Build -> IO Build
-- progress dockerService build = do
--     pTraceIO $ show build
--     case buildState build of
--         BuildReady ->
--             case buildHasNextStep build of
--                 Left result -> do
--                     TIO.putStrLn "Transitioning to `BuildFinished` state"
--                     pure $
--                         build
--                             { buildState = BuildFinished result
--                             }
--                 Right step -> do
--                     TIO.putStrLn "Attempt starting container..."
--                     let script' = T.unlines $ "set -ex" : NE.toList (stepCommands step)
--                     let image' = stepImage step
--                     let createOptions =
--                             Docker.CreateContainerOptions
--                                 { image = image'
--                                 , script = script'
--                                 }
--                     containerId' <-
--                         either
--                             -- TODO: decide how to handle errors
--                             ( \err -> do
--                                 TIO.putStrLn $ "/!\\ Failed to create container: " <> T.pack (show err)
--                                 throwIO $ userError "Failed to create container: "
--                             )
--                             pure
--                             =<< Docker.createContainer dockerService createOptions
--                     Docker.startContainer dockerService containerId'
--                     TIO.putStrLn "... container started! Transitioning to `BuildRunning` state"
--                     pure $
--                         build
--                             { buildState =
--                                 BuildRunning $
--                                     BuildRunningState
--                                         { currentStepName = stepName step
--                                         , containerId = containerId'
--                                         }
--                             }
--         BuildRunning state -> do
--             TIO.putStrLn "Container is running, checking status for failure..."
--             status <- containerStatus dockerService (containerId state)
--             case status of
--                 Right Docker.ContainerRunning -> do
--                     TIO.putStrLn "Container is still running, NOOP"
--                     pure build
--                 Right (Docker.ContainerExited exitCode) -> do
--                     let stepResult = exitCodeToStepResult exitCode
--                     pure
--                         build
--                             { buildState =
--                                 case stepResult of
--                                     StepFailed _ -> BuildFinished BuildFailed
--                                     StepSucceeded -> BuildFinished BuildSuccess
--                             , buildCompletedSteps =
--                                 Map.insert
--                                     (currentStepName state)
--                                     stepResult
--                                     (buildCompletedSteps build)
--                             }
--                 Right (Docker.ContainerUnknownStatus status') -> do
--                     TIO.putStrLn $ "Unexpected container status: " <> status'
--                     pure $
--                         build{buildState = BuildFinished BuildFailed}
--                 Left err -> do
--                     TIO.putStrLn $ "Unexpected error: " <> T.pack (show err)
--                     pure $ build{buildState = BuildFinished BuildFailed}
--         BuildFinished _ -> do
--             pure build

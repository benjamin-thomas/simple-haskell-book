{-# LANGUAGE OverloadedStrings #-}

module CoreSpec (spec) where

import Control.Monad (unless)
import Core
    ( Build (..)
    , BuildResult (..)
    , BuildState (..)
    , Pipeline (..)
    , Step (..)
    , StepName (..)
    , StepResult (..)
    )
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Docker
import Runner (Service (prepareBuild, runBuild))
import qualified Runner
import System.Process.Typed (proc, readProcess_, runProcess_)
import Test.Hspec (Spec, afterAll_, describe, it, shouldBe)

makeStep :: Text -> Text -> NonEmpty Text -> Step
makeStep name image commands =
    Step
        { stepName = StepName name
        , stepImage = Docker.Image image
        , stepCommands = commands
        }

makePipeline :: NonEmpty Step -> Pipeline
makePipeline steps =
    Pipeline
        { pipelineSteps = steps
        , pipelineState = BuildReady
        }

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runnerService = do
    result <-
        runBuild runnerService
            . prepareBuild runnerService
            $ makePipeline steps

    buildState result `shouldBe` BuildFinished BuildSuccess
    Map.elems (buildCompletedSteps result) `shouldBe` [StepSucceeded, StepSucceeded]
  where
    steps :: NonEmpty Step
    steps =
        (:|)
            (makeStep "First step" "ubuntu" (NE.singleton "date"))
            [makeStep "Second step" "ubuntu" (NE.singleton "uname -r")]

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

spec :: Spec
spec = do
    let dockerService = Docker.createService
    let runnerService = Runner.createService dockerService
    describe "Core" $
        afterAll_ deleteQuadContainersExn $
            it "should run a successful build" $
                testRunSuccess runnerService

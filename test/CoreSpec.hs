{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE ScopedTypeVariables #-}

module CoreSpec (spec) where

import Control.Concurrent (threadDelay)
import Core
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Docker
import Test.Hspec (Spec, describe, it, shouldBe)

makeStep :: Text -> Text -> NonEmpty Text -> Step
makeStep name image commands =
    Step
        { stepName = StepName name
        , stepCommands = commands
        , stepImage = Docker.Image image
        }

makePipeline :: NonEmpty Step -> Pipeline
makePipeline steps =
    Pipeline{pipelineSteps = steps}

-- Test values
testPipeline :: Pipeline
testPipeline =
    makePipeline $
        makeStep "First step" "ubuntu" (NE.singleton "date")
            :| [makeStep "Second step" "ubuntu" (NE.singleton "uname -r")]

testBuild :: Build
testBuild =
    Build
        { buildPipeline = testPipeline
        , buildState = BuildReady
        , buildCompletedSteps = mempty
        }

runBuild :: Docker.Service -> Build -> IO Build
runBuild dockerService build = do
    newBuild <- Core.progress dockerService build
    case buildState newBuild of
        BuildFinished _ -> pure newBuild
        _ -> do
            threadDelay $ 1 * second
            runBuild dockerService newBuild
  where
    second = 1000 * 1000

testRunSuccess :: Docker.Service -> IO ()
testRunSuccess dockerService = do
    result <- runBuild dockerService testBuild
    buildState result `shouldBe` BuildFinished BuildSuccess
    Map.elems (buildCompletedSteps result) `shouldBe` [StepSucceeded, StepSucceeded]

spec :: Spec
spec = do
    describe "Core" $ do
        let dockerService = Docker.createService
        it "should run a successful build" $ do
            testRunSuccess dockerService

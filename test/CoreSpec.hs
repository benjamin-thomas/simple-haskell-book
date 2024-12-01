{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE ScopedTypeVariables #-}

module CoreSpec (spec) where

import Core
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Docker
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "Core" $ do
        it "can be built" $ do
            True `shouldBe` True

-- Helper functions
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
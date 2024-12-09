module Runner
where

-- ( createService
-- , Service
--     ( runBuild
--     , prepareBuild
--     )
-- )

import Control.Concurrent (threadDelay)
import qualified Core
import qualified Docker

-- data Service
--     = Service
--     { prepareBuild :: Core.Pipeline -> Core.Build
--     , runBuild :: Core.Build -> IO Core.Build
--     }

-- createService :: Docker.Service -> Service
-- createService dockerService =
--     Service
--         { prepareBuild = prepareBuild_
--         , runBuild = runBuild_ dockerService
--         }

-- prepareBuild_ :: Core.Pipeline -> Core.Build
-- prepareBuild_ pipeline =
--     Core.Build
--         { Core.buildPipeline = pipeline
--         , Core.buildState = Core.BuildReady
--         , Core.buildCompletedSteps = mempty
--         }

-- runBuild_ :: Docker.Service -> Core.Build -> IO Core.Build
-- runBuild_ dockerService build = do
--     newBuild <- Core.progress dockerService build
--     case Core.buildState newBuild of
--         Core.BuildFinished _ -> pure newBuild
--         _ -> do
--             threadDelay $ 200 * millisec
--             runBuild_ dockerService newBuild
--   where
--     millisec = 1000
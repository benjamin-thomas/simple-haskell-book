-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where

import qualified CoreSpec

{-

hspec-discover doesn't seem to play well with tests requiring setup in IO.

 -}
-- main :: IO ()
-- main = do
--     CoreSpec.spec

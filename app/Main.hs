module Main where

import Network.Wai.Handler.Warp

import qualified Lib as L
import qualified Swag as S

main :: IO ()
main = do
  putStrLn "freewill.it running on http://localhost:8080/"
  run 8080 S.app

module Main where
import           Init.Prelude
import           Test.Tasty

main :: IO ()
main =
  defaultMain $ testGroup "init" mempty

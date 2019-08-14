module Main where

import Parser
import Runner

import Control.Monad.State
import Text.ParserCombinators.ReadP
import System.Environment

import qualified Data.Text as Text
import qualified Data.Text.IO as IOText
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do
  (fileName:_) <- getArgs
  fileContent <- C8.readFile fileName
  let [(program, [])] = readP_to_S parseProgram $ C8.unpack fileContent
      device = createDevice program
  putStrLn $ show program
  let (ret, dev) = runState runDevice device
  mapM IOText.putStr ret
  putStrLn ""
  putStrLn $ show dev
  return ()


module Main where

import Parser
import Runner
import Types

import Control.Monad.State
import Text.ParserCombinators.ReadP
import System.Environment

import Control.Monad (zipWithM)

import qualified Data.Text as Text
import qualified Data.Text.IO as IOText
import qualified Data.ByteString.Char8 as C8

printProgram program = do
  putStrLn "Strings:"
  mapM (putStrLn . show) $ tableEntries $ programStringTable program
  putStrLn []
  putStrLn "Symbols:"
  mapM (putStrLn . show) $ tableEntries $ programSymbolTable program
  putStrLn []
  zipWithM (\num program -> do putStr "Program "
                               putStr $ show num
                               putStrLn ":"
                               mapM (putStrLn . show . snd) $ snd program
                               putStrLn [])

           [1..]
           $ tapesData $ programCode program

main :: IO ()
main = do
  (fileName:_) <- getArgs
  fileContent <- C8.readFile fileName
  let [(program, [])] = readP_to_S parseProgram $ C8.unpack fileContent
      device = createDevice program
  printProgram program
  let (ret, dev) = runState runDevice device
  mapM IOText.putStr ret
  putStrLn ""
  putStrLn $ show dev
  return ()


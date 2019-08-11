{-# LANGUAGE OverloadedStrings #-}
module Runner where

-- PUSHI\|SUB\|PRINT\|PUSHSTR\|PUSHUNIT

import Types

import Control.Monad.State

import Data.Int (Int64)
import Data.List (head, tail, splitAt, elemIndex)
import Data.Char (chr)
import Data.Bits (xor)
import Data.Maybe (isNothing)
import Data.Vector ((!), (//))

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as Text

type ProgramCounter = (Int, Int)
type CallStack = [ProgramCounter]
type EnvironmentSymbols = Map.Map Int64 (Maybe ValueStackItem)
type EnvironmentSymbolsStacks = V.Vector EnvironmentSymbols
type EnvironmentSymbolsStackIndex = Int
type EnvironmentSymbolsStack = [EnvironmentSymbolsStackIndex]
data ValueStackItem = ValueStackItemInt Int64
                    | ValueStackItemString Int
                    | ValueStackItemFloat Double 
                    | ValueStackList [ValueStackItem]
                    | ValueStackItemClosure Int64 EnvironmentSymbolsStackIndex deriving (Show, Eq)
type ValueStack = [ValueStackItem]

data LisbyDevice = LisbyDevice { deviceProgram :: LisbyProgram
                               , devicePC :: Maybe ProgramCounter
                               , deviceCallStack :: CallStack
                               , deviceSymbolsStack :: EnvironmentSymbolsStack
                               , deviceSymbolsStacks :: EnvironmentSymbolsStacks
                               , deviceValueStack :: ValueStack } deriving Show

createDevice program = LisbyDevice program (Just (0, 0)) [] [0] (V.singleton Map.empty) []

incPC :: Monad m => StateT LisbyDevice m ()
incPC = do
  dev <- get
  case devicePC dev of
    Just (tape, row) -> put $ dev { devicePC = Just (tape, row + 1) }
    Nothing -> return ()

getInstruction :: LisbyDevice -> Maybe TapeOpcode
getInstruction dev = let Just (tape, row) = devicePC dev
                         codes = tapesData $ programCode $ deviceProgram dev
                         (tapeSize, tapeCode) = codes !! tape
                       in if (fromIntegral tape) < (tapesSize $ programCode $ deviceProgram dev) &&
                             (fromIntegral row) < tapeSize
                            then Just $ snd $ tapeCode !! row
                            else Nothing

executeInstruction :: Monad m => TapeOpcode -> StateT LisbyDevice m (Maybe Text.Text)
executeInstruction PRINT = do
  dev <- get
  let (val:rest) = deviceValueStack dev
  put $ dev { deviceValueStack = rest }
  incPC
  return $ case val of
    ValueStackItemInt i -> Just $ Text.pack $ [chr $ fromIntegral i]
    ValueStackItemString i -> Just $ (tableEntries $ programStringTable $ deviceProgram dev) !! i
    ValueStackItemFloat f -> Just $ Text.pack $ show f
    ValueStackList l -> Just $ Text.pack $ show l
    ValueStackItemClosure i _ -> Just $ Text.pack $ "Closure address " ++ show i

executeInstruction (PUSHI v) = do
  dev <- get
  put $ dev { deviceValueStack = ((ValueStackItemInt v):deviceValueStack dev) }
  incPC
  return Nothing

executeInstruction SUB = do
  dev <- get
  let val1:val2:rest = deviceValueStack dev
      doOp (ValueStackItemInt i) (ValueStackItemInt i2) = return $ ValueStackItemInt $ i - i2
      doOp (ValueStackItemFloat f) (ValueStackItemFloat f2) = return $ ValueStackItemFloat $ f - f2
      doOp _ _ = error "Can't sub different value types" 
  subbed <- doOp val1 val2
  put $ dev { deviceValueStack = subbed:rest }
  incPC
  return Nothing

executeInstruction ADD = do
  dev <- get
  let val1:val2:rest = deviceValueStack dev
      doOp (ValueStackItemInt i) (ValueStackItemInt i2) = return $ ValueStackItemInt $ i + i2
      doOp (ValueStackItemFloat f) (ValueStackItemFloat f2) = return $ ValueStackItemFloat $ f + f2
      doOp _ _ = error "Can't add different value types" 
  subbed <- doOp val1 val2
  put $ dev { deviceValueStack = subbed:rest }
  incPC
  return Nothing

executeInstruction XOR = do
  dev <- get
  let val1:val2:rest = deviceValueStack dev
      doOp (ValueStackItemInt i) (ValueStackItemInt i2) = return $ ValueStackItemInt $ xor i i2
      doOp _ _ = error "Can't xor different value types" 
  subbed <- doOp val1 val2
  put $ dev { deviceValueStack = subbed:rest }
  incPC
  return Nothing

executeInstruction MOD = do
  dev <- get
  let val1:val2:rest = deviceValueStack dev
      doOp (ValueStackItemInt i) (ValueStackItemInt i2) = return $ ValueStackItemInt $ mod i i2
      doOp _ _ = error "Can't mod different value types" 
  subbed <- doOp val1 val2
  put $ dev { deviceValueStack = subbed:rest }
  incPC
  return Nothing

executeInstruction POP = do
  dev <- get
  put $ dev { deviceValueStack = tail $ deviceValueStack dev }
  incPC
  return Nothing

executeInstruction Types.EQ = do
  dev <- get
  let val1:val2:rest = deviceValueStack dev
      result = if val1 == val2
                 then ValueStackItemInt 1
                 else ValueStackItemInt 0
  put $ dev { deviceValueStack = result:rest }
  incPC
  return Nothing

executeInstruction (JT offset) = do
  dev <- get
  let val:rest = deviceValueStack dev
  put $ dev { deviceValueStack = rest }
  if val == ValueStackItemInt 1
    then executeInstruction (JMP offset)
    else do
      incPC
      return Nothing
executeInstruction (JF offset) = do
  dev <- get
  let val:rest = deviceValueStack dev
  put $ dev { deviceValueStack = rest }
  if val == ValueStackItemInt 0
    then executeInstruction (JMP offset)
    else do
      incPC
      return Nothing

executeInstruction (PUSHSTR i) = do
  dev <- get
  put $ dev { deviceValueStack = (ValueStackItemString $ fromIntegral i):deviceValueStack dev }
  incPC
  return Nothing

executeInstruction PUSHUNIT = do
  dev <- get
  put $ dev { deviceValueStack = (ValueStackList []):deviceValueStack dev }
  incPC
  return Nothing

executeInstruction PUSHTRUE = do
  dev <- get
  put $ dev { deviceValueStack = (ValueStackItemInt 1):deviceValueStack dev }
  incPC
  return Nothing

executeInstruction PUSHFALSE = do
  dev <- get
  put $ dev { deviceValueStack = (ValueStackItemInt 0):deviceValueStack dev }
  incPC
  return Nothing

executeInstruction (HALT) = do
  dev <- get
  put $ dev { devicePC = Nothing }
  return $ Just $ Text.pack $ show dev
  -- return Nothing

executeInstruction (LIST c) = do
  dev <- get
  let (before,after) = splitAt (fromIntegral c) $ deviceValueStack dev
  put $ dev { deviceValueStack = ((ValueStackList before):after) }
  incPC
  return Nothing

executeInstruction LISTCAT = do
  dev <- get
  let (ValueStackList list1):(ValueStackList list2):rest = deviceValueStack dev
  put $ dev { deviceValueStack = (ValueStackList $ list2 ++ list1):rest }
  incPC
  return Nothing

executeInstruction (DECLARE i) = do
  dev <- get
  let currEnvIdx:_ = deviceSymbolsStack dev
      currEnv = deviceSymbolsStacks dev ! currEnvIdx
      newEnv = Map.insert i Nothing currEnv
      newEnvStacks = deviceSymbolsStacks dev // [(currEnvIdx, newEnv)]
  put $ dev { deviceSymbolsStacks = newEnvStacks }
  incPC
  return Nothing

executeInstruction (STORE i) = do
  dev <- get
  let val:rest = deviceValueStack dev
      findSymbol [] = Nothing
      findSymbol (symbols:rest) = case Map.lookup i (deviceSymbolsStacks dev ! symbols) of
                                    val@(Just _) -> Just symbols
                                    Nothing -> findSymbol rest
      Just symbolIdx = findSymbol $ deviceSymbolsStack dev
      symbolEnv = deviceSymbolsStacks dev ! symbolIdx
      newEnv = Map.update (\_ -> Just $ Just val) i symbolEnv
      newEnvStacks = deviceSymbolsStacks dev // [(symbolIdx, newEnv)]
  put $ dev { deviceSymbolsStacks = newEnvStacks, deviceValueStack = rest }
  incPC
  return Nothing

executeInstruction (STORETOP i) = do
  dev <- get
  let val:rest = deviceValueStack dev
      oldTop = deviceSymbolsStacks dev ! 0
      newTop = Map.update (\_ -> Just $ Just val) i oldTop
      newEnvStacks = deviceSymbolsStacks dev // [(0, newTop)]
  put $ dev { deviceValueStack = rest, deviceSymbolsStacks = newEnvStacks }
  incPC
  return Nothing

executeInstruction (PUSHSY i) = do
  dev <- get
  let findSymbolValue [] = Nothing
      findSymbolValue (symbols:rest) = case Map.lookup i (deviceSymbolsStacks dev ! symbols) of
                                         val@(Just _) -> val
                                         Nothing -> findSymbolValue rest
      Just (Just val) = findSymbolValue $ deviceSymbolsStack dev
  put $ dev { deviceValueStack = val:deviceValueStack dev }
  incPC
  return $ Just $ Text.concat [(tableEntries $ programSymbolTable $ deviceProgram dev) !! fromIntegral i, " = ", Text.pack $ show val]
  return Nothing

executeInstruction (PUSHCLOSURE i) = do
  dev <- get
  put $ dev { deviceValueStack = (ValueStackItemClosure i $ head $ deviceSymbolsStack dev):deviceValueStack dev }
  incPC
  return Nothing

executeInstruction NEWENV = do
  dev <- get
  let newStacks = V.snoc (deviceSymbolsStacks dev) Map.empty
      newStackIdx = V.length newStacks - 1
  put $ dev { deviceSymbolsStack = newStackIdx:deviceSymbolsStack dev, deviceSymbolsStacks = newStacks }
  incPC
  return Nothing

executeInstruction CALL = do
  dev <- get
  let (ValueStackItemClosure val symbols):rest = deviceValueStack dev
      Just (currTape, currRow) = devicePC dev
  put $ dev { devicePC = Just (fromIntegral val, 0)
            , deviceValueStack = rest
            , deviceSymbolsStack = symbols:deviceSymbolsStack dev
            , deviceCallStack = (currTape, currRow + 1):deviceCallStack dev }
  return Nothing

executeInstruction RET = do
  dev <- get
  let newPc:callStack = deviceCallStack dev
  put $ dev { devicePC = Just newPc, deviceCallStack = callStack, deviceSymbolsStack = tail $ deviceSymbolsStack dev }
  return Nothing

executeInstruction DEPARTENV = do
  dev <- get
  put $ dev { deviceSymbolsStack = tail $ deviceSymbolsStack dev }
  incPC
  return Nothing

executeInstruction (JMP i) = do
  dev <- get
  let Just (tape, row) = devicePC dev
      findInstruction idx ((at, _):rest) = if at == i
                                           then idx
                                           else findInstruction (idx + 1) rest
      currProgram = snd $ (tapesData $ programCode $ deviceProgram dev) !! tape
      targetIdx = findInstruction 0 currProgram
  put $ dev { devicePC = Just (tape, targetIdx) }
  return Nothing

executeInstruction HEAD = do
  dev <- get
  let ValueStackList (l1:_):rest = deviceValueStack dev
  put $ dev { deviceValueStack = (l1:rest) }
  incPC
  return Nothing

executeInstruction TAIL = do
  dev <- get
  let ValueStackList (_:tail):rest = deviceValueStack dev
  put $ dev { deviceValueStack = ((ValueStackList tail):rest) }
  incPC
  return Nothing

-- executeInstruction instr = incPC >> (return $ Just $ Text.pack $ (show instr ++ "\n"))
executeInstruction instr = error $ "Instruction " ++ (show instr) ++ " not implemented"

runDevice :: Monad m => StateT LisbyDevice m [Text.Text]
-- runSingleInstruction :: Monad m => StateT LisbyDevice m TapeOpcode
runDevice = do
  dev <- get
  if isNothing $ devicePC dev
    then return []
    else do
      let instruction = getInstruction dev
      case instruction of
        Nothing -> return []
        Just instruction' -> do
          values <- executeInstruction instruction'
          rest <- runDevice
          case values of
            --Nothing -> return $ (Text.concat [Text.pack $ show $ devicePC dev, ": ", Text.pack $ show instruction', "\n"]):rest
            Nothing -> return rest
            --Just val -> return $ (Text.concat [Text.pack $ show $ devicePC dev, ": ", Text.pack $ show instruction', ": ", val, "\n"]):rest
            Just val -> return $ val:rest

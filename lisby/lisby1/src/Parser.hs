module Parser
    where

import Types

import Control.Applicative
import Text.ParserCombinators.ReadP

import Data.Int (Int64)
import Data.Foldable (foldlM)
import Data.Bits (shiftL)
import Data.Char (ord)
import Data.Text.Encoding (decodeUtf8')
import Data.Binary.Get (runGet, getDoublebe)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as Text


parseOp :: ReadP (TapeOpcode)
parseOp = do
  opcode <- get
  opcode' <- case ord opcode of 
    0 -> return HALT

    1 -> return ADD
    2 -> return SUB
    3 -> return MUL
    4 -> return DIV
    5 -> return XOR
    6 -> return MOD

    7 -> return AND
    8 -> return OR

    9 -> return INV

    10 -> parseInt64 >>= return . PUSHI
    -- 10 -> parseInt64 >>= return . PUSHI
    11 -> parseFloat >>= return . PUSHF
    12 -> parseInt64 >>= return . PUSHSTR

    13 -> parseInt64 >>= return . PUSHSY




    14 -> parseInt64 >>= return . PUSHSYRAW

    15 -> return PUSHTRUE
    16 -> return PUSHFALSE
    17 -> return PUSHUNIT

    18 -> parseInt64 >>= return . PUSHCLOSURE




    19 -> parseInt64 >>= return . PUSHCONT


    20 -> parseInt64 >>= return . QUOTED
    21 -> return POP
    22 -> return CALL


    23 -> return TAILCALL
    24 -> return RET


    25 -> parseInt64 >>= return . JT
    26 -> parseInt64 >>= return . JF
    27 -> parseInt64 >>= return . JMP

    28 -> parseInt64 >>= return . STORE


    29 -> parseInt64 >>= return . STORETOP



    30 -> return Types.EQ
    31 -> return NEQ
    32 -> return Types.GT
    33 -> return GE
    34 -> return Types.LT
    35 -> return LE
    36 -> return NOT


    37 -> parseInt64 >>= return . DECLARE

    38 -> return PRINT


    39 -> parseInt64 >>= return . LIST
    40 -> return HEAD

    41 -> return TAIL

    42 -> return LISTCAT

    43 -> return EVAL
    44 -> return DUMP
    45 -> return NEWENV

    46 -> return DEPARTENV
    _ -> error "Invalid program"

  return opcode'

opSize (PUSHI _) = 9
opSize (PUSHF _) = 9
opSize (PUSHSTR _) = 9
opSize (PUSHSY _) = 9
opSize (STORE _) = 9
opSize (STORETOP _) = 9
opSize (PUSHCLOSURE _) = 9
opSize (JT _) = 9
opSize (JF _) = 9
opSize (JMP _) = 9
opSize (DECLARE _) = 9
opSize (LIST _) = 9
opSize (PUSHSYRAW _) = 9
opSize (QUOTED _) = 9
opSize (PUSHCONT _) = 9
opSize _ = 1

parseInt64 :: ReadP (Int64)
parseInt64 = do
  be <- mapM (\_ -> get) [1..8]
  val <- foldlM (\i b -> return $ (shiftL i 8) + (fromIntegral $ ord b)) 0 $ reverse be
  return val

parseFloat :: ReadP (Double)
parseFloat = do
  chars <- mapM (\_ -> get) [1..8]
  return $ runGet getDoublebe (LC8.pack chars)

parseTable :: ReadP Table
parseTable = do
  val <- parseInt64
  entries <- flip mapM [1..val] $ \_ -> do
    len <- parseInt64
    let addChar before = do
           chr <- get
           let before' = C8.snoc before chr
           case decodeUtf8' before' of
             Right text -> if (fromIntegral len) == (fromIntegral $ Text.length text)
                             then return text
                             else addChar before'
             Left _ -> addChar before'
    text <- addChar C8.empty
    return text
                                      
  return $ Table entries

parseTapes :: ReadP Tapes
parseTapes = do
  val <- parseInt64
  entries <- flip mapM [1..val] $ \_ -> do
    len <- parseInt64
    let parseOps at = if at == len
                        then return []
                        else do
                          op <- parseOp
                          rest <- parseOps (at + opSize op)
                          return $ (at, op):rest
    code <- parseOps 0
    return (len, code)
  return $ Tapes val entries

parseProgram :: ReadP LisbyProgram
parseProgram = do
  string "LISBY001"
  firstTable <- parseTable
  secondTable <- parseTable
  code <- parseTapes
  string "100YBSIL"
  return $ LisbyProgram firstTable secondTable code

module Types where

import Data.Int (Int64)

import qualified Data.Text as Text

data Table = Table { tableEntries :: [Text.Text]} deriving Show

data TapeOpcode = HALT       -- ceases execution of the program

                | ADD       -- binary arithmetic; pop two values from value stack;
                | SUB       -- and then push the result back; should only work
                | MUL       -- with integers and floats
                | DIV     
                | XOR       -- integer xor
                | MOD       -- integer and float modulo

                | AND       -- conditionals; pop two values from value stack;
                | OR        -- push the result back; AND and OR should both be short-circuiting

                | INV       -- pop value from stack; do bitwise inversions; works on integers

                | PUSHI Int64   -- push an integer to the value stack
                | PUSHF Double  -- push a float
                | PUSHSTR Int64 -- string table reference follows (int); the respective string
                                -- index is pushed into the value stack stack
                | PUSHSY Int64  -- symbol table reference follows (int); the respective symbol
                                -- value within the currently active environment is pushed
                                -- into the value stack; this PUSH should not
                                -- be preceded by QUOTED.

                | PUSHSYRAW Int64 -- symbol table reference follows (int); the respective
                                  -- symbol index is pushed into the value stack
                | PUSHTRUE        -- push a boolean true to value stack
                | PUSHFALSE       -- push a boolean false to value stack
                | PUSHUNIT        -- push unit (empty list) to value stack

                | PUSHCLOSURE Int64 -- pushes a closure reference to the stack; the reference
                                    -- means a tape identifier; the closure closes over the
                                    -- present environment, which becomes its parent
                                    -- environment

                | PUSHCONT Int64 -- pushes the current continuation to stack; the continuation
                                 -- captures the current state of computation completely

                | QUOTED Int64   -- increases quoting level of next value push
                | POP            -- pops a value from the stack without storing it
                | CALL           -- pops a closure from value stack; transfers control there;
                                 -- a fresh environment is activated with the closure
                                 -- environment as its parent
                | TAILCALL       -- like call except tail call
                | RET            -- pops a return address from call stack; transfers control
                                 -- there; the call site's environment is activated

                | JT Int64  -- pops a boolean from value stack, jumps to tape offset if true
                | JF Int64  -- pops a boolean from value stack, jumps to tape offset if false
                | JMP Int64 -- unconditional jump to tape offset

                | STORE Int64    -- stores popped stack value to given symbol index of the
                                 -- presently active environment; the symbol must be DECLAREd
                                 -- in the presently active or any of its parent environments
                | STORETOP Int64 -- stores popped stack value to given symbol index of the
                                 -- top-level environment; should error if the symbol index
                                 -- is not defined in the top-level environment

                | EQ         -- conditionals: pop two values from value stack, and push
                | NEQ        -- a boolean value back depending on the result
                | GT     
                | GE     
                | LT     
                | LE     
                | NOT       -- pop boolean from value stack; if popped value is TRUE, push back
                            -- FALSE and vice versa

                | DECLARE Int64 -- declares a variable with the given symbol index (int) in the
                                -- presently active environment
                | PRINT         -- pops one value and attempts to display it to the controlling
                                -- terminal

                | LIST Int64    -- constructs a list of N entries popped from value stack
                | HEAD          -- pops a list and pushes its first element; should error if
                                -- the list is empty
                | TAIL          -- pops a list and pushes it without its first element; should
                                -- error if the list is empty
                | LISTCAT       -- pops two lists from value stack; pushes their concatenation

                | EVAL       -- pops a value, evaluates it in an empty environment
                | DUMP       -- dumps the current vm status in an implementation defined manner
                | NEWENV       -- activates a fresh environment with the current environment as
                               -- its parent
                | DEPARTENV       -- departs the current environment and activates the parent
                    -- environment
                  deriving (Show, Eq)

data Tapes = Tapes { tapesSize :: Int64
                   , tapesData :: [(Int64, [(Int64, TapeOpcode)])] } deriving Show

data LisbyProgram = LisbyProgram { programStringTable :: Table
                                 , programSymbolTable :: Table
                                 , programCode :: Tapes} deriving Show

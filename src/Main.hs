{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Control.Monad.State.Lazy
import           Control.Monad.Trans
import           Data.Char
import           Data.Map.Strict
import           System.Environment

data Brainerror = MissingClosingBracket
                | MissingOpeningBracket
                deriving Show

data Brainstate = Brainstate
  { tapeValues      :: Map Int Int
  , pointerLocation :: Int
  , nestedLoopNo    :: Int
  }

data Brainfuck state err next = IncrementPointer next
                              | DecrementPointer next
                              | IncrementValue next
                              | DecrementValue next
                              | OutputChar next
                              | InputChar next
                              | LoopStart next
                              | LoopEnd next
                              | Continue next
                              | Throw err
                              | Terminate
                              deriving Functor

makeFree ''Brainfuck

type BrainfuckOp = Brainfuck Brainstate Brainerror
type BrainfuckIO = StateT Brainstate IO
type Brainfree = Free BrainfuckOp

parse :: [Char] -> Brainfree ()
parse []       = terminate
parse (x : xs) = tokenize x >> parse xs

tokenize :: Char -> Brainfree ()
tokenize '>' = incrementPointer
tokenize '<' = decrementPointer
tokenize '+' = incrementValue
tokenize '-' = decrementValue
tokenize ',' = inputChar
tokenize '.' = outputChar
tokenize '[' = loopStart
tokenize ']' = loopEnd
tokenize _   = continue

interpretJmp :: Brainfree next -> BrainfuckIO (Brainfree next)
interpretJmp x = undefined

-- Returns start of the loop
-- Once it completes
interpretLoop :: Brainfree next -> BrainfuckIO (Brainfree next)
interpretLoop x = do
  -- Stop once it reaches end of loop
  interpret x
  return x

interpret :: Brainfree next -> BrainfuckIO ()
interpret (Free (IncrementPointer next)) = do
  v <- gets pointerLocation
  modify (\s -> s { pointerLocation = v + 1 } )
  interpret next
interpret (Free (DecrementPointer next)) = do
  v <- gets pointerLocation
  let v' = if v - 1 < 0 then 0 else v - 1
  modify (\s -> s { pointerLocation = v' } )
  interpret next
interpret (Free (IncrementValue next)) = do
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
  modify (\s -> s { tapeValues =  insert l (v + 1) m })
  interpret next
interpret (Free (DecrementValue next)) = do
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
      v' = if v - 1 < 0 then 0 else v - 1
  modify (\s -> s { tapeValues =  insert l (v') m })
  interpret next
interpret (Free (OutputChar next)) = do
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
  liftIO $ putChar (chr v)
  interpret next
interpret (Free (InputChar next)) = do
  c <- liftIO getChar
  l <- gets pointerLocation
  m <- gets tapeValues
  modify (\s -> s { tapeValues =  insert l (ord c) m })
  interpret next
interpret (Free (Continue next)) = interpret next
interpret (Free (LoopStart next)) = undefined
interpret (Free (LoopEnd next)) = undefined
interpret (Free Terminate) = do
  ln <- gets nestedLoopNo
  case ln == 0 of
    True -> return ()
    False -> interpret $ throw MissingClosingBracket
interpret (Free (Throw err)) = error $ show err


initialState :: Brainstate
initialState = Brainstate empty 0 0

main :: IO ()
main = do
  (f : _) <- getArgs
  code <- readFile f
  _ <- runStateT (interpret $ parse code) initialState
  return ()

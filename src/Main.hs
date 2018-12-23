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
                | MissingOpeninBracket

data Brainstate = Brainstate
  { tapeValues      :: Map Int Int
  , pointerLocation :: Int
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
                              deriving Functor

makeFree ''Brainfuck

type BrainfuckOp = Brainfuck Brainstate Brainerror
type BrainfuckIO = StateT Brainstate IO
type Brainfree = Free BrainfuckOp

parse :: [Char] -> Brainfree ()
parse []       = return ()
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

interpret :: BrainfuckOp (BrainfuckIO next) -> BrainfuckIO next
interpret (IncrementPointer next) = do
  v <- gets pointerLocation
  modify (\s -> s { pointerLocation = v + 1 } )
  next
interpret (DecrementPointer next) = do
  v <- gets pointerLocation
  let v' = if v - 1 < 0 then 0 else v - 1
  modify (\s -> s { pointerLocation = v' } )
  next
interpret (IncrementValue next) = do
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
  modify (\s -> s { tapeValues =  insert l (v + 1) m })
  next
interpret (DecrementValue next) = do
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
      v' = if v - 1 < 0 then 0 else v - 1
  modify (\s -> s { tapeValues =  insert l (v') m })
  next
interpret (OutputChar next) = do
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
  liftIO $ putChar (chr v)
  next
interpret (InputChar next) = do
  c <- liftIO getChar
  l <- gets pointerLocation
  m <- gets tapeValues
  modify (\s -> s { tapeValues =  insert l (ord c) m })
  next
interpret (Continue next) = next

initialState :: Brainstate
initialState = Brainstate empty 0

main :: IO ()
main = do
  (f : _) <- getArgs
  code <- readFile f
  _ <- runStateT (iterM interpret $ parse code) initialState
  return ()

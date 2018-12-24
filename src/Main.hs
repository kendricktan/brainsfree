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
  , nestedLoopJmp   :: Int
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

-- Inteprets '[' if byte at data pointer is 0
--
interpretJmp :: Brainfree next -> BrainfuckIO (Brainfree next)
interpretJmp (Free (IncrementPointer next)) = interpretJmp next
interpretJmp (Free (DecrementPointer next)) = interpretJmp next
interpretJmp (Free (IncrementValue next))   = interpretJmp next
interpretJmp (Free (DecrementValue next))   = interpretJmp next
interpretJmp (Free (InputChar next))        = interpretJmp next
interpretJmp (Free (OutputChar next))       = interpretJmp next
interpretJmp (Free (LoopStart next))        = do
  ln <- gets nestedLoopJmp
  modify (\s -> s { nestedLoopJmp = ln + 1 })
  interpretJmp next
interpretJmp (Free (LoopEnd next))          = do
  ln <- gets nestedLoopJmp
  let ln' = ln - 1
  modify (\s -> s { nestedLoopJmp = ln' })
  case ln' of
    0 -> do
      return next
    _ -> interpretJmp next
interpretJmp (Free (Continue next))         = interpretJmp next
interpretJmp (Free Terminate)               = interpret $ throw MissingClosingBracket

-- Inteprets '[' if byte at data pointer is non-zero
-- Returns end of the loop
-- Once it completes
interpretLoop :: Brainfree next -> BrainfuckIO (Brainfree next)
interpretLoop x = do
  -- Stop once it reaches end of loop
  end <- interpret x
  return end

interpret :: Brainfree next -> BrainfuckIO (Brainfree next)
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
  liftIO $ putStrLn "Enter a character: "
  (c: _) <- liftIO getLine
  l <- gets pointerLocation
  m <- gets tapeValues
  modify (\s -> s { tapeValues =  insert l (ord c) m })
  interpret next
interpret (Free (Continue next)) = interpret next
interpret loopStart@(Free (LoopStart next)) = do
  ln <- gets nestedLoopNo
  l <- gets pointerLocation
  m <- gets tapeValues
  let v = findWithDefault 0 l m
  case v == 0 of
    True -> do
      loopEnd <- interpretJmp loopStart
      interpret loopEnd
    False -> do
      modify (\s -> s { nestedLoopNo = ln + 1 } )
      -- Execute till loop ends
      loopEnd <- interpretLoop next
      -- Read current pointer position
      l' <- gets pointerLocation
      m' <- gets tapeValues
      let v' = findWithDefault 0 l' m'
      -- If byte at data pointer is nonzero,
      -- jump back to start
      case v' /= 0 of
        True  -> interpret loopStart
        False -> interpret loopEnd
interpret (Free (LoopEnd next)) = do
  ln <- gets nestedLoopNo
  let ln' = ln - 1
  case ln' < 0 of
    -- Incorrect number of brackets
    True -> interpret $ throw MissingOpeningBracket
    False -> do
      modify (\s -> s { nestedLoopNo = ln' })
      return next
interpret (Free Terminate) = do
  ln <- gets nestedLoopNo
  case ln == 0 of
    True  -> return terminate
    False -> interpret $ throw MissingClosingBracket
interpret (Free (Throw err)) = error $ show err


initialState :: Brainstate
initialState = Brainstate empty 0 0 0

main :: IO ()
main = do
  (f : _) <- getArgs
  code <- readFile f
  _ <- runStateT (interpret $ parse code) initialState
  return ()

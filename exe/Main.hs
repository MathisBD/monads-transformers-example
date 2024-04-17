{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT (StateT), runStateT)
import Control.Monad.Trans.Writer (WriterT (WriterT), runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)

-- A custom error type.
data MyError
  = MyError1 Int String
  | MyError2 Int Int
  deriving (Eq, Show)

-- A stupid configuration type.
data Config = Config {configA :: Int, configB :: String}
  deriving (Eq, Show)

-- A big monad stack. The first line is not important.
-- The second line is what we care about : which typeclasses our monad instantiates.
newtype MyMonad a = MyMonad (WriterT [String] (StateT Int (ReaderT Config (Either MyError))) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Int, MonadError MyError, MonadWriter [String])

-- A function to run the monad.
-- This is the ONLY function that needs to know in what order we composed the monad transformers.
runMyMonad :: Config -> Int -> MyMonad a -> Either MyError (a, Int, [String])
runMyMonad config state (MyMonad m) =
  let result = runReaderT (runStateT (runWriterT m) state) config
   in case result of
        Left err -> Left err
        Right ((x, logs), state') -> Right (x, state', logs)

-- Simple example of using the monad.
example :: Int -> MyMonad Int
example x = do
  -- Log the argument.
  tell ["Got argument", show x]
  -- Check the argument is not null.
  when (x == 0) $
    throwError (MyError1 x "Got a null argument (dumb error)...")
  -- Get some parameters in the config.
  tell ["Getting config"]
  config <- asks configA
  -- Check the argument is not equal to the config parameter.
  when (x == config) $
    throwError (MyError2 x config)
  -- Get the Int state.
  state <- get
  -- Increment the state.
  tell ["Incrementing the state", show state]
  put $ state + 1
  -- Return a dummy value.
  tell ["Done"]
  return $ state + x

main :: IO ()
main = do
  -- Choose some parameters to run the monadic computation.
  let state = 25
  let config = Config {configA = 10, configB = "I am configB"}
  -- Run the monadic computation and inspect the result.
  case runMyMonad config state $ example 42 of
    Left err ->
      -- Print the error.
      putStrLn $ "Error: " <> show err
    Right (res, _, logs) -> do
      -- Print the result .
      putStrLn $ "Got result: " <> show res
      -- Print the logs.
      putStrLn "Logs:"
      forM_ logs $ \msg -> putStrLn $ "  " <> msg

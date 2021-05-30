module Terminal
  (withTerminal,
   TerminalArg(..))
where

import System.Console.ANSI
import System.IO
import Control.Exception (catch, throw, SomeException)

data TerminalArg
  = Name String
  | NoEcho
  deriving (Eq)

withTerminal :: [TerminalArg] -> IO () -> IO ()
withTerminal args k = do
  saveTitle -- Hack while I don't figure out how to save terminal state.
  parseArgs args
  catch (do k; resetTerminal)
    -- Reset terminal even if something bad happens.
    (\e -> do resetTerminal; throw (e :: SomeException))

parseArgs :: [TerminalArg] -> IO ()
parseArgs [] = return ()
parseArgs (Name name : args) = do setTitle name
                                  parseArgs args
parseArgs (NoEcho : args) = do hSetEcho stdin False

-- Problem: doesn't actually revert to previous state, just to the state
-- thought to be generally convenient, so probably not nestable.
-- Should be rewritten so that I can e.g. do withTerminal [NoEcho] draw state
-- and the cursor doesn't flicker and so on.
resetTerminal :: IO ()
resetTerminal = do
  setSGR [Reset]
  restoreTitle
  -- Put the cursor at lowest line we can, so screen isn't cut.
  setCursorPosition 999 0
  putStrLn ""
  showCursor

-- ANSI sequences I didn't find on the library.
saveTitle    :: IO ()
restoreTitle :: IO ()

saveTitle    = hPutStr stdout "\ESC[22;0t"
restoreTitle = hPutStr stdout "\ESC[23;0t"

{- Copyright 2011-2012 Zac Meadows

  This file is part of sunWM.

  sunWM is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  sunWM is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with sunWM.  If not, see <http://www.gnu.org/licenses/>.


-- --------------------------------------------------------------------------
-- Module      :  SunWM.Util
-- Copyright   :  (c) Zac Meadows 2012
-- License     :  GPL3 (see LICENSE)
--
-- Maintainer  :  zmeadows@gmail.com
--
-- Utility functions required for various extraneous purposes.
-- --------------------------------------------------------------------------
-}
{-# LANGUAGE BangPatterns #-}
module Sunwm.Util where

import Control.Monad.IO.Class
import System.Posix.Signals
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.IO
import System.Posix.IO
import System.Posix.Types (ProcessID)
import Control.Exception.Extensible (try, finally, SomeException(..))
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Maybe (isJust)

-- | Ignore SIGPIPE to avoid termination when a pipe is full, and SIGCHLD to
-- avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: MonadIO m => m ()
installSignalHandlers = liftIO $ do
    _ <- installHandler openEndedPipe Ignore Nothing
    _ <- installHandler sigCHLD Ignore Nothing
    _ <- (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = liftIO $ do
    _ <- installHandler openEndedPipe Default Nothing
    _ <- installHandler sigCHLD Default Nothing
    return ()

spawn :: MonadIO m => String -> m ()
spawn !c = spawnPID c >>= \_ -> return ()

-- | Like 'spawn', but returns the 'ProcessID' of the launched application
spawnPID :: MonadIO m => String -> m ProcessID
spawnPID !x = xfork $ executeFile "/bin/sh" False ["-c", x] Nothing

-- | A replacement for 'forkProcess' which resets default signal handlers.
xfork :: MonadIO m => IO () -> m ProcessID
xfork proc = liftIO . forkProcess . finally nullStdin $
    uninstallSignalHandlers >> createSession >> proc
 where nullStdin = do
         fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
         dupTo fd stdInput >> closeFd fd

spawnPipe :: String -> IO Handle
spawnPipe !x = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    _ <- forkProcess $ do
        _ <- createSession
        _ <- uninstallSignalHandlers
        _ <- dupTo rd stdInput
        executeFile "/bin/sh" False ["-c", x] Nothing
    return h

-- | dmenu spawner
dmenu :: MonadIO m => String -> String -> String -> String -> String -> m ()
dmenu fn nb nf sb sf = spawn $ concat
      [ "dmenu_run "
      , "-l 10 "
      , "-nb '" ++ nb ++ "' "
      , "-nb '" ++ nb ++ "' "
      , "-sb '" ++ sb ++ "' "
      , "-sf '" ++ sf ++ "' "
      , "-nf '" ++ nf ++ "' "
      , "-fn '" ++ fn ++ "' "]



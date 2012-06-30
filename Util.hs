module Util where

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
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = liftIO $ do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing
    return ()

spawn :: MonadIO m => String -> m ()
spawn c = spawnPID c >>= \_ -> return ()

-- | Like 'spawn', but returns the 'ProcessID' of the launched application
spawnPID :: MonadIO m => String -> m ProcessID
spawnPID x = xfork $ executeFile "/bin/sh" False ["-c", x] Nothing

-- | A replacement for 'forkProcess' which resets default signal handlers.
xfork :: MonadIO m => IO () -> m ProcessID
xfork x = liftIO . forkProcess . finally nullStdin $ do
                uninstallSignalHandlers
                createSession
                x
 where nullStdin = do
         fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
         dupTo fd stdInput
         closeFd fd

spawnPipe :: String -> IO Handle
spawnPipe x = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    forkProcess $ do
        createSession
        uninstallSignalHandlers
        dupTo rd stdInput
        executeFile "/bin/sh" False ["-c", x] Nothing
    return h


{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DeriveDataTypeable, TemplateHaskell, TypeOperators #-}

module Sunwm.Plugins.Utils.Dmenu where

import Sunwm.Core
import System.Process

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

dmenu :: String -> String -> String -> String -> String -> SUN ()
dmenu fn nb nf sb sf = liftIO $ void $ runCommand $ concat
      [ "dmenu_run "
      , "-l 10 "
      , "-nb '" ++ nb ++ "' "
      , "-nb '" ++ nb ++ "' "
      , "-sb '" ++ sb ++ "' "
      , "-sf '" ++ sf ++ "' "
      , "-nf '" ++ nf ++ "' "
      , "-fn '" ++ fn ++ "' "]

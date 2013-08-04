module Sunwm.Plugin where

import Sunwm.STree
import Sunwm.Core

import Data.Dynamic
import qualified Data.Map.Strict as M
import Data.Label.PureM ((=.),gets)
import Control.Applicative ((<$>))
import Data.Maybe
import Control.Monad.IO.Class

putPD :: Typeable a => a -> SUN ()
putPD s = do
    gets pluginState >>= liftIO . print
    pluginState =. M.insert (typeOf s) (toDyn s)
    gets pluginState >>= liftIO . print

getPD :: Typeable a => TypeRep -> SUN a
getPD tr = do
    pd <- fromJust <$> (M.lookup tr <$> gets pluginState)
    let Just p = fromDynamic pd
    return p

-- getsPS :: (PluginState -> b) -> SUN b
-- getsPS = flip fmap getPS

modifyPS :: Typeable a => TypeRep -> (a -> a) -> SUN ()
modifyPS tr f = getPD tr >>= putPD . f

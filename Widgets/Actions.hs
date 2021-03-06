{-
Copyright 2011 Alexander Midgley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Widgets.Actions where

import Control.Monad
import Control.Monad.State
import Data.IORef

import Widgets.Types

newtype UIAction a = UIAction (StateT Widgets IO a)
    deriving (Monad, MonadIO, MonadState Widgets)

runAction :: IORef Widgets -> UIAction a -> IO a
runAction mainWidgetRef (UIAction action) =
    readIORef mainWidgetRef >>= runStateT action >>= \(result, state) ->
        writeIORef mainWidgetRef state >> return result


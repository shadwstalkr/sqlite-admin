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

{-# LANGUAGE FlexibleContexts #-}

module Widgets.MainWindow
    ( loadMainWinDef
    , mainWin  -- from Widgets.Types
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import SqliteAdmin
import Widgets.Actions
import Widgets.InfoContainer
import Widgets.Types
import Widgets.Utils

loadMainWinDef :: IORef SqliteDb -> FilePath -> IO Widgets
loadMainWinDef dbRef gladePath = do
  Just xml <- xmlNew gladePath
  mainWidget <- Widgets <$> xmlGetWidget xml castToWindow "mainWindow"
                        <*> xmlGetWidget xml castToNotebook "infoContainer"
  mainWidgetRef <- newIORef mainWidget
  setMenuActions mainWidgetRef dbRef xml

  readIORef mainWidgetRef

setMenuActions :: IORef Widgets -> IORef SqliteDb -> GladeXML -> IO ()
setMenuActions mainWidgetRef dbRef xml = do
  openItem <- xmlGetWidget xml castToMenuItem "menuFileOpen"
  quitItem <- xmlGetWidget xml castToMenuItem "menuFileQuit"

  onActivateLeaf openItem . runAction mainWidgetRef $ onFileOpen dbRef
  onActivateLeaf quitItem mainQuit

  return ()

onFileOpen :: (MonadState Widgets m, MonadIO m) => IORef SqliteDb -> m ()
onFileOpen dbRef = fileName >>= maybe (return ()) openFile
    where
      fileName = do
        parent <- gets mainWin
        getOpenFilename parent "Open SQLite Database"

      openFile path = do
        db <- liftIO $ openDb path
        liftIO $ writeIORef dbRef db
        onDbChanged db

onDbChanged SqliteDbClosed = clearInfoPages
onDbChanged db = do
  clearInfoPages
  addDbPage db
  return ()

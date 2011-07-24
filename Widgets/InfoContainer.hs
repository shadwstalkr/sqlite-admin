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

module Widgets.InfoContainer where

import Control.Monad.State
import Graphics.UI.Gtk hiding (get)
import System.FilePath

import SqliteAdmin
import Widgets.Actions
import Widgets.DbTree
import Widgets.TableBrowser
import Widgets.Types

activateTable :: (MonadIO m, MonadState Widgets m) => SqliteDb -> String -> m ()
activateTable SqliteDbClosed _ = return ()
activateTable db table = do
  btn <- liftIO $ buttonNewWithLabel "Test"
  liftIO $ widgetShow btn
  info <- gets infoContainer
  liftIO $ notebookAppendPage info btn table >>= notebookSetCurrentPage info
  return ()

clearInfoPages :: (MonadIO m, MonadState Widgets m) => m ()
clearInfoPages = gets infoContainer >>= liftIO . doClear >> return ()
    where doClear info = notebookGetNPages info >>= (flip replicateM) (notebookRemovePage info 0)

addDbPage :: (MonadIO m, MonadState Widgets m) => SqliteDb -> m Int
addDbPage db = do
  actionRunner <- runAction `liftM` gets widgetsRef
  addInfoPage name =<< newDbTree db (actionRunner . addBrowseTablePage db)
    where name = takeFileName . dbPath $ db

addBrowseTablePage :: (MonadIO m, MonadState Widgets m) =>
                      SqliteDb -> String -> m ()
addBrowseTablePage db table = do
  addInfoPage table =<< newTableBrowser db table
  return ()

addInfoPage :: (MonadIO m, MonadState Widgets m, WidgetClass w) => String -> w -> m Int
addInfoPage title widget = liftIO . doAdd =<< gets infoContainer
    where
      doAdd container = do
        widgetShow widget
        page <- notebookAppendPage container widget title
        notebookSetCurrentPage container page
        return page

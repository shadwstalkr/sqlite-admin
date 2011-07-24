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

module Widgets.TableBrowser (newTableBrowser) where

import Control.Arrow
import Control.Monad.Trans
import Graphics.UI.Gtk

import SqliteAdmin
import Sqlite.Types
import Widgets.Utils

newTableBrowser :: (MonadIO m) => SqliteDb -> String -> m ScrolledWindow
newTableBrowser db table = liftIO $ do
  view <- treeViewNew
  let Just dbColumns = lookup table . dbTables $ db
  rowStore <- listStoreNew =<< getAllRows db table
  treeViewSetModel view rowStore
  addViewColumns view rowStore dbColumns

  -- Add the view to a scrolled window
  win <- scrolledWindowNew Nothing Nothing
  containerAdd win view
  widgetShow view

  return win

addViewColumns :: (TypedTreeModelClass model, TreeModelClass (model [SqliteValue])) =>
                  TreeView -> model [SqliteValue] -> [ColumnDesc] -> IO ()
addViewColumns view model = mapM_ addColumn . zip [0..]
    where
      addColumn (colIdx, dbColumn) = do
        (col, rend) <- addTextColumn view
        treeViewColumnSetTitle col $ colName dbColumn
        cellLayoutSetAttributes col rend model $ \row -> [cellText := getSqliteText (row !! colIdx)]

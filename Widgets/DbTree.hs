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

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Widgets.DbTree
    ( newDbTree
    ) where

import Control.Monad.State
import Data.Tree
import Graphics.UI.Gtk

import SqliteAdmin
import Widgets.Types

data DbTreeRow = TableRow {rowName :: String}
               | ColumnRow {rowName :: String}

type DbTree = Tree DbTreeRow
type DbForest = Forest DbTreeRow

newDbTree :: (MonadIO m) =>
             SqliteDb ->
             m TreeView

newDbTree db = liftIO $ do
  view <- treeViewNew
  (nameCol, nameRenderer) <- addTextColumn view
  treeStore <- treeStoreNew model
  treeViewSetModel view treeStore
  cellLayoutSetAttributes nameCol nameRenderer treeStore $ \row -> [cellText := rowName row]
  return view

  where
    model = dbTree db
  

dbTree :: SqliteDb -> DbForest
dbTree = map tableNode . dbTables
    where
      tableNode (tableName, columns) = Node (TableRow tableName) (map columnNode columns)
      columnNode column = Node (ColumnRow (colName column)) []


addTextColumn = flip addColumn cellRendererTextNew

addColumn view rendererNew = do
  col <- treeViewColumnNew
  renderer <- rendererNew
  cellLayoutPackStart col renderer True
  treeViewAppendColumn view col
  return (col, renderer)

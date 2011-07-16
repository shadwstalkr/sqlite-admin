module StructureTree
    ( structureTree
    , setupStructureView
    , setStructureViewDb
    , setStructureViewModel
    ) where

import Control.Applicative
import Control.Monad
import Data.Tree
import Graphics.UI.Gtk
import System.FilePath

import SqliteAdmin


data StructureTreeRow = DbName {rowName :: String}
                      | TableName {rowName :: String}

structureTree :: SqliteDb -> Tree StructureTreeRow
structureTree db =
    let tableNames = map fst . dbTables $ db
        dbName = takeFileName . dbPath $ db
        leaf name = (TableName name, [])
    in Node (DbName dbName) $ unfoldForest leaf tableNames

setupStructureView :: TreeView -> IO ()
setupStructureView = addNameColumn

setStructureViewDb :: SqliteDb -> TreeView -> IO ()
setStructureViewDb db view = treeStoreNew ([structureTree db]) >>= setStructureViewModel view

setStructureViewModel :: TreeView -> TreeStore StructureTreeRow -> IO ()
setStructureViewModel view model = do
  treeViewSetModel view model
  join $ mapM_ setupRenderers <$> treeViewGetColumns view

    where
      setupRenderers col = do
        renderers <- treeViewColumnGetCellRenderers col
        mapM_ (setupNameRenderer model col . castToCellRendererText) renderers

addNameColumn :: TreeView -> IO ()
addNameColumn view = do
  nameCol <- treeViewColumnNew
  nameRenderer <- cellRendererTextNew
  cellLayoutPackStart nameCol nameRenderer True
  treeViewAppendColumn view nameCol
  return ()

setupNameRenderer model column renderer =
    cellLayoutSetAttributes column renderer model $ \row -> [cellText := rowName row]
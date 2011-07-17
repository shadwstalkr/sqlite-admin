module StructureTree
    ( StructureTreeView
    , structureTree
    , setupStructureView
    , setStructureViewDb
    , setOnTableRowActivated
    ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Tree
import Graphics.UI.Gtk
import System.FilePath

import SqliteAdmin

type StructureTreeCallback = SqliteDb -> String -> IO ()

data StructureTreeRow = DbName {rowName :: String}
                      | TableName {rowName :: String, activatedClbk :: StructureTreeCallback}

data StructureTreeView = StructureTreeView
    { treeView :: TreeView
    , rowActivatedSignal :: Maybe (ConnectId TreeView)
    , onTableRowActivated :: StructureTreeCallback
    }

structureTree :: SqliteDb -> StructureTreeView -> Tree StructureTreeRow
structureTree db view =
    let tblClbk = onTableRowActivated view
        tableNames = map fst . dbTables $ db
        dbName = takeFileName . dbPath $ db
        leaf name = (TableName name tblClbk, [])
    in Node (DbName dbName) $ unfoldForest leaf tableNames

setupStructureView :: TreeView -> IO StructureTreeView
setupStructureView view = do
  addNameColumn view
  return $! StructureTreeView view Nothing nullCallback

setOnTableRowActivated :: StructureTreeView -> StructureTreeCallback -> StructureTreeView
setOnTableRowActivated view callback = view {onTableRowActivated = callback}

setStructureViewDb :: SqliteDb -> StructureTreeView -> IO StructureTreeView
setStructureViewDb db view = do
  let model = [structureTree db view]
  treeStore <- treeStoreNew model
  treeViewSetModel (treeView view) treeStore
  join $ mapM_ (setupRenderers treeStore) <$> treeViewGetColumns (treeView view)
  setCallbacks view db model

    where
      setupRenderers model col = do
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

setCallbacks :: StructureTreeView -> SqliteDb -> Forest StructureTreeRow -> IO StructureTreeView
setCallbacks view SqliteDbClosed _ = return view
setCallbacks view db model = do
  maybe (return ()) signalDisconnect $ rowActivatedSignal view
  activateId <- onRowActivated (treeView view) (rowActivatedHandler db model)
  return view {rowActivatedSignal = Just activateId}

rowActivatedHandler :: SqliteDb -> Forest StructureTreeRow -> TreePath -> TreeViewColumn -> IO ()
rowActivatedHandler db model path _ = maybe (return ()) dispatch $ modelAt model path
    where dispatch (DbName _) = return ()
          dispatch row = (activatedClbk row) db (rowName row)

nullCallback :: StructureTreeCallback
nullCallback _ _ = return ()

modelAt :: Forest StructureTreeRow -> TreePath -> Maybe StructureTreeRow
modelAt _ [] = Nothing
modelAt model (idx:idxs) = rootLabel <$> foldl' step (Just $ model !! idx) idxs
    where
      step Nothing _ = Nothing
      step (Just node) idx =
          let children = subForest node
          in if idx < length children then
                 Just $ children !! idx
             else Nothing

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

module Main where

import Control.Applicative
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Tree

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
--import Graphics.UI.Gtk.Builder

import SqliteAdmin
import StructureTree

data MainWindowWidget = MainWindowWidget {
      mainWin :: Window,
      structureView :: TreeView
    }

newtype UIAction a = UIAction (ReaderT MainWindowWidget IO a)
    deriving (Monad, MonadIO, MonadReader MainWindowWidget)

runAction :: MainWindowWidget -> UIAction a -> IO a
runAction mainWidget (UIAction action) = runReaderT action mainWidget

main = do
  initGUI

  dbRef <- newIORef SqliteDbClosed

  mainWidget <- loadMainWinDef dbRef "mainWindow.glade"
--  mainWidget <- loadMainWinDef2 "mainWindow2.glade"
  onDestroy (mainWin mainWidget) mainQuit

  widgetShowAll (mainWin mainWidget)
  mainGUI

loadMainWinDef :: IORef SqliteDb -> FilePath -> IO MainWindowWidget
loadMainWinDef dbRef gladePath = do
  Just xml <- xmlNew gladePath
  mainWidget <- MainWindowWidget <$> xmlGetWidget xml castToWindow "mainWindow"
                                 <*> xmlGetWidget xml castToTreeView "structureView"
  setMenuActions mainWidget dbRef xml
  setupStructureView $ structureView mainWidget

  return mainWidget

setMenuActions :: MainWindowWidget -> IORef SqliteDb -> GladeXML -> IO ()
setMenuActions mainWidget dbRef xml = do
  openItem <- xmlGetWidget xml castToMenuItem "menuFileOpen"
  quitItem <- xmlGetWidget xml castToMenuItem "menuFileQuit"

  onActivateLeaf openItem . runAction mainWidget $ onFileOpen dbRef
  onActivateLeaf quitItem mainQuit

  return ()

onFileOpen :: IORef SqliteDb -> UIAction ()
onFileOpen dbRef = fileName >>= maybe (return ()) openFile
    where
      fileName = do
        parent <- asks mainWin
        liftIO $ getOpenFilename parent "Open SQLite Database"

      openFile path = do
        db <- liftIO $ openDb path
        liftIO $ writeIORef dbRef db
        onDbChanged db

onDbChanged :: SqliteDb -> UIAction ()
onDbChanged SqliteDbClosed = return ()
onDbChanged db = asks structureView >>= liftIO . setStructureViewDb db
          
getOpenFilename :: Window -> String -> IO (Maybe String)
getOpenFilename parent title = do
  openDlg <- fileChooserDialogNew
                 (Just title)
                 (Just parent)
                 FileChooserActionOpen
                 [("gtk-cancel" ,ResponseCancel), ("gtk-open", ResponseAccept)]

  widgetShow openDlg
  response <- dialogRun openDlg
  let result = case response of
                 ResponseAccept -> fileChooserGetFilename openDlg
                 _              -> return Nothing
  result <* widgetHide openDlg

{-
loadMainWinDef2 path = do
  builder <- builderNew
  builderAddFromFile builder path
  MainWindowWidget <$> builderGetObject builder castToWindow "mainWindow"
                   <*> builderGetObject builder castToTreeView "structureView"
-}

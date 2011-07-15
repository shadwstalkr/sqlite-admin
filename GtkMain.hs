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

module Main where

import Control.Applicative
import Data.IORef
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
--import Graphics.UI.Gtk.Builder

import SqliteAdmin

data MainWindowWidget = MainWindowWidget {
      mainWin :: Window,
      structureView :: TreeView
    }

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

  return mainWidget

setMenuActions :: MainWindowWidget -> IORef SqliteDb -> GladeXML -> IO ()
setMenuActions mainWidget dbRef xml = do
  openItem <- xmlGetWidget xml castToMenuItem "menuFileOpen"
  quitItem <- xmlGetWidget xml castToMenuItem "menuFileQuit"

  onActivateLeaf openItem $ onFileOpen mainWidget dbRef
  onActivateLeaf quitItem mainQuit

  return ()

onFileOpen :: MainWindowWidget -> IORef SqliteDb -> IO ()
onFileOpen mainWidget dbRef = fileName >>= maybe (return ()) openFile
    where
      fileName = getOpenFilename (mainWin mainWidget) "Open SQLite Database"

      openFile path = do
        db <- openDb path
        writeIORef dbRef db
        onDbChanged mainWidget db

onDbChanged :: MainWindowWidget -> SqliteDb -> IO ()
onDbChanged mainWidget db = return ()
          
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

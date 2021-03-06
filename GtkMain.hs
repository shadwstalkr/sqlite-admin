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
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Tree

import Graphics.UI.Gtk

import SqliteAdmin
import Widgets.MainWindow

main = do
  initGUI

  dbRef <- newIORef SqliteDbClosed

  mainWidget <- loadMainWinDef dbRef "mainWindow.glade"
--  mainWidget <- loadMainWinDef2 "mainWindow2.glade"
  onDestroy (mainWin mainWidget) mainQuit

  widgetShowAll (mainWin mainWidget)
  mainGUI


{-
loadMainWinDef2 path = do
  builder <- builderNew
  builderAddFromFile builder path
  MainWindowWidget <$> builderGetObject builder castToWindow "mainWindow"
                   <*> builderGetObject builder castToTreeView "structureView"
-}

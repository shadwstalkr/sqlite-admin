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

module Widgets.Utils where

import Control.Applicative
import Control.Monad.Trans
import Graphics.UI.Gtk

getOpenFilename :: (MonadIO m) => Window -> String -> m (Maybe String)
getOpenFilename parent title = liftIO $ do
  openDlg <- fileChooserDialogNew
                 (Just title)
                 (Just parent)
                 FileChooserActionOpen
                 [("gtk-cancel", ResponseCancel), ("gtk-open", ResponseAccept)]

  widgetShow openDlg
  response <- dialogRun openDlg
  let result = case response of
                 ResponseAccept -> fileChooserGetFilename openDlg
                 _              -> return Nothing
  result <* widgetHide openDlg

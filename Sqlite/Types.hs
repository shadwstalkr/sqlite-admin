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

module Sqlite.Types where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3

data SqliteValType = SqliteNull
                   | SqliteInt
                   | SqliteReal
                   | SqliteText
                   | SqliteBlob
                     deriving (Eq, Show)
                   
data ColumnDesc = ColumnDesc
                  {
                    colName :: String,
                    colType :: SqliteValType,
                    colKey :: Bool,
                    colNullable :: Bool
                  }
                  deriving (Eq, Show)

type TableDesc = (String,        -- Table name
                  [ColumnDesc])  -- Columns

data SqliteDb = SqliteDbClosed
              | SqliteDb
                {
                  dbPath   :: FilePath,
                  dbConn   :: Connection,
                  dbTables :: [TableDesc]
                }

instance Show SqliteDb where
    show SqliteDbClosed = "SqliteDbClosed"
    show db = "SqliteDb " ++ show (dbPath db)

findCol :: String -> [ColumnDesc] -> Maybe ColumnDesc
findCol name = find (\col -> name == colName col)

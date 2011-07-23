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

module SqliteAdmin
    ( SqliteDb(SqliteDbClosed)
    , openDb
    )
where

import Control.Applicative
import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import Sqlite.Parser
import Sqlite.Types

openDb :: FilePath -> IO SqliteDb
openDb path = do
  conn <- connectSqlite3 path
  schema <- readSchema conn
  return $! SqliteDb path conn schema

readSchema :: Connection -> IO [TableDesc]
readSchema conn = mapM getTableDesc =<< getTables conn
    where
      getTableDesc table = (,) <$> pure table <*> describeSqliteTable conn table

describeSqliteTable :: Connection -> String -> IO [ColumnDesc]
describeSqliteTable conn table = map parseColumnRow <$> rows
    where
      rows = quickQuery conn ("PRAGMA table_info(" ++ table ++ ")") []

parseColumnRow (_:name:typeName:notNull:_:pKey:_) = ColumnDesc (fromSql name)
                                                               (parseTypeName (fromSql typeName))
                                                               (fromSql pKey)
                                                               (not (fromSql notNull))

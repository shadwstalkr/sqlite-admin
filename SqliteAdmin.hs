module SqliteAdmin
    ( SqliteDb(..)
    )
where

import Control.Applicative
import Control.Monad
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
                    colUnique :: Bool,
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

openDb :: FilePath -> SqliteDb
openDb = undefined

readSchema :: Connection -> IO [TableDesc]
readSchema conn = mapM getTableDesc =<< getTables conn
    where
      getTableDesc table = (,) <$> pure table <*> describeSqliteTable conn table

describeSqliteTable :: Connection -> String -> IO [ColumnDesc]
describeSqliteTable conn table = return []

findCol :: String -> [ColumnDesc] -> Maybe ColumnDesc
findCol name = find (\col -> name == colName col)

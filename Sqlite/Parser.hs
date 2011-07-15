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

module Sqlite.Parser (parseCreateTable, parseTypeName) where

import Control.Applicative hiding (optional)
import Data.Char
import Text.ParserCombinators.Parsec hiding ((<|>))

import Sqlite.Types

parseCreateTable :: String -> Either ParseError [ColumnDesc]
parseCreateTable = parse createTableStmt "(unknown)" . dropWhile isSpace . map toLower

parseTypeName :: String -> SqliteValType
parseTypeName = either (const SqliteBlob) id . parsed
    where
      parsed = parse typeName "(unknown)" . dropWhile isSpace . map toLower

createTableStmt = create *> name *> columnList <* spaces <* char ';'

create = string "create" *> spaces *> string "table" *> spaces

name = many1 (alphaNum <|> oneOf "_") <* spaces

columnList = between (char '(' <* spaces) (char ')') $ columnDef `sepBy` (char ',' <* spaces)

columnDef = ColumnDesc <$> name
                       <*> typeName <* spaces
                       <*> isPrimaryKey
                       <*> isNotNullable

typeName = option (SqliteBlob) $ SqliteInt <$ (try intTypeName)
                             <|> SqliteReal <$ (try realTypeName)
                             <|> SqliteText <$ (try textTypeName)
                             <|> SqliteBlob <$ (try blobTypeName)

isPrimaryKey = option False (True <$ try primaryKey)

isNotNullable = option False $ True <$ try (string "not" <* spaces <* string "null" <* spaces)

intTypeName = string "int" <* optional (string "eger")
              <|> string "tinyint"
              <|> string "smallint"
              <|> string "mediumint"
              <|> string "bigint"
              <|> string "unsigned" *> spaces *> string "big" *> spaces *> string "int"
              <|> string "int2"
              <|> string "int8"

realTypeName = string "real"
               <|> string "double" <* optional (spaces *> string "precision")
               <|> string "float"

textTypeName = string "character" <* optional size
               <|> string "varchar" <* optional size
               <|> string "nchar" <* optional size
               <|> string "nvarchar" <* optional size
               <|> string "text"
               <|> string "clob"
               <|> string "string"

blobTypeName = string "blob"

size = spaces *> between (char '(' <* spaces) (char ')') (many1 digit)

primaryKey = string "primary" <* spaces
             <* string "key" <* spaces
             <* order <* spaces
             <* optional (string "autoincrement") <* spaces
    where
      order = optional (string "asc" <|> string "desc")
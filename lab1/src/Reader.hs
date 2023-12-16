module Reader
  ( getReader,
    createReader,
    updateReader,
    deleteReader,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Reader = Reader {readerId :: Int, firstName :: String, lastName :: String, email :: String}
  deriving (Show)

instance FromRow Reader where
  fromRow = Reader <$> field <*> field <*> field <*> field

getReader :: Connection -> Int -> IO [Reader]
getReader conn rid = query conn "SELECT * FROM readers WHERE reader_id = ?" (Only rid)

createReader :: Connection -> String -> String -> String -> IO [Reader]
createReader conn firstName lastName email = do
  query conn "INSERT INTO readers (first_name, last_name, email) VALUES (?, ?, ?) RETURNING *" (firstName, lastName, email)

updateReader :: Connection -> Int -> String -> String -> String -> IO [Reader]
updateReader conn rid firstName lastName email = do
  query conn "UPDATE readers SET first_name = ?, last_name = ?, email = ? WHERE reader_id = ? RETURNING *" (firstName, lastName, email, rid)

deleteReader :: Connection -> Int -> IO Bool
deleteReader conn rid = do
  n <- execute conn "DELETE FROM readers WHERE reader_id = ?" (Only rid)
  return $ n > 0

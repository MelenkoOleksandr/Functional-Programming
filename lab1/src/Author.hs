module Author
  ( getAuthor,
    createAuthor,
    updateAuthor,
    deleteAuthor,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Author = Author {authorId :: Int, firstName :: String, lastName :: String}
  deriving (Show)

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

getAuthor :: Connection -> Int -> IO [Author]
getAuthor conn aid = query conn "SELECT * FROM authors WHERE author_id = ?" (Only aid)

createAuthor :: Connection -> String -> String -> IO [Author]
createAuthor conn firstName lastName = do
  query conn "INSERT INTO authors (first_name, last_name) VALUES (?, ?) RETURNING *" (firstName, lastName)

updateAuthor :: Connection -> Int -> String -> String -> IO [Author]
updateAuthor conn aid firstName lastName = do
  query conn "UPDATE authors SET first_name = ?, last_name = ? WHERE author_id = ? RETURNING *" (firstName, lastName, aid)

deleteAuthor :: Connection -> Int -> IO Bool
deleteAuthor conn aid = do
  n <- execute conn "DELETE FROM authors WHERE author_id = ?" (Only aid)
  return $ n > 0

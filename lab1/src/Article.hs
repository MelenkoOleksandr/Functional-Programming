module Article
  ( getArticle,
    createArticle,
    updateArticle,
    deleteArticle,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Article = Article {articleId :: Int, title :: String, authorId :: Int, content :: String, annotation :: String, publicationFiles :: [String]}
  deriving (Show)

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field <*> field <*> field <*> field

getArticle :: Connection -> Int -> IO [Article]
getArticle conn aid = query conn "SELECT * FROM articles WHERE article_id = ?" (Only aid)

createArticle :: Connection -> String -> Int -> String -> String -> [String] -> IO [Article]
createArticle conn title authorId content annotation publicationFiles = do
  query conn "INSERT INTO articles (title, author_id, content, annotation, publication_files) VALUES (?, ?, ?, ?, ?) RETURNING *" (title, authorId, content, annotation, publicationFiles)

updateArticle :: Connection -> Int -> String -> Int -> String -> String -> [String] -> IO [Article]
updateArticle conn aid title authorId content annotation publicationFiles = do
  query conn "UPDATE articles SET title = ?, author_id = ?, content = ?, annotation = ?, publication_files = ? WHERE article_id = ? RETURNING *" (title, authorId, content, annotation, publicationFiles, aid)

deleteArticle :: Connection -> Int -> IO Bool
deleteArticle conn aid = do
  n <- execute conn "DELETE FROM articles WHERE article_id = ?" (Only aid)
  return $ n > 0

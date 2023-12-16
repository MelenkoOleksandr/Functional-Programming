module Review
  ( getReview,
    createReview,
    updateReview,
    deleteReview,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Review = Review {reviewId :: Int, articleId :: Int, readerId :: Int, reviewText :: String, reviewDate :: UTCTime}
  deriving (Show)

instance FromRow Review where
  fromRow = Review <$> field <*> field <*> field <*> field <*> field

getReview :: Connection -> Int -> IO [Review]
getReview conn rvid = query conn "SELECT * FROM reviews WHERE review_id = ?" (Only rvid)

createReview :: Connection -> Int -> Int -> String -> IO [Review]
createReview conn articleId readerId reviewText = do
  query conn "INSERT INTO reviews (article_id, reader_id, review_text) VALUES (?, ?, ?) RETURNING *" (articleId, readerId, reviewText)

updateReview :: Connection -> Int -> Int -> String -> IO [Review]
updateReview conn rvid articleId readerId reviewText = do
  query conn "UPDATE reviews SET article_id = ?, reader_id = ?, review_text = ? WHERE review_id = ? RETURNING *" (articleId, readerId, reviewText, rvid)

deleteReview :: Connection -> Int -> IO Bool
deleteReview conn rvid = do
  n <- execute conn "DELETE FROM reviews WHERE review_id = ?" (Only rvid)
  return $ n > 0

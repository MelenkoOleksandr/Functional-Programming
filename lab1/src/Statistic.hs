module Statistic
  ( getStatistic,
    createStatistic,
    updateStatistic,
    deleteStatistic,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data Statistic = Statistic {statisticId :: Int, articleId :: Int, viewsCount :: Int, reviewsCount :: Int}
  deriving (Show)

instance FromRow Statistic where
  fromRow = Statistic <$> field <*> field <*> field <*> field

getStatistic :: Connection -> Int -> IO [Statistic]
getStatistic conn sid = query conn "SELECT * FROM statistics WHERE statistic_id = ?" (Only sid)

createStatistic :: Connection -> Int -> Int -> Int -> IO [Statistic]
createStatistic conn articleId viewsCount reviewsCount = do
  query conn "INSERT INTO statistics (article_id, views_count, reviews_count) VALUES (?, ?, ?) RETURNING *" (articleId, viewsCount, reviewsCount)

updateStatistic :: Connection -> Int -> Int -> Int -> IO [Statistic]
updateStatistic conn sid articleId viewsCount reviewsCount = do
  query conn "UPDATE statistics SET article_id = ?, views_count = ?, reviews_count = ? WHERE statistic_id = ? RETURNING *" (articleId, viewsCount, reviewsCount, sid)

deleteStatistic :: Connection -> Int -> IO Bool
deleteStatistic conn sid = do
  n <- execute conn "DELETE FROM statistics WHERE statistic_id = ?" (Only sid)
  return $ n > 0

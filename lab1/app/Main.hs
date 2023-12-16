module Main (main) where

import Article
import Author
import Database.PostgreSQL.Simple
import Reader
import Review
import Statistic

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectDatabase = "postgres",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

main :: IO ()
main = do
  conn <- connect localPG
  putStrLn "Author's first name? "
  authorFirstName <- getLine
  putStrLn "Author's last name? "
  authorLastName <- getLine
  newAuthor <- createAuthor conn authorFirstName authorLastName
  putStrLn $ "New Author: " ++ show newAuthor

  firstAuthor <- getAuthor conn 1
  putStrLn $ "First Author: " ++ show firstAuthor

  isAuthorDeleted <- deleteAuthor conn 1
  putStrLn $ "First Author deleted: " ++ show isAuthorDeleted

  firstAuthor <- getAuthor conn 1
  putStrLn $ "First Author: " ++ show firstAuthor

  secondAuthor <- getAuthor conn 2
  putStrLn $ "Second Author: " ++ show secondAuthor

  updatedSecondAuthor <- updateAuthor conn 2 "NewFirstName" "NewLastName"
  putStrLn $ "Updated Author: " ++ show updatedSecondAuthor
  
  close conn

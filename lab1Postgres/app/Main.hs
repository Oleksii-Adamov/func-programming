import Database.PostgreSQL.Simple

main :: IO ()
main = do
  -- Replace these with your actual database connection parameters
  let connectionString = "host=localhost dbname=mydatabase user=myuser password=mypassword"

  -- Connect to the PostgreSQL database
  conn <- connectPostgreSQL connectionString

  -- Execute a sample query
  results <- query_ conn "SELECT 2 + 2"

  -- Print the result
  case results of
    [Only answer] -> putStrLn $ "Result: " ++ show (answer :: Int)
    _ -> putStrLn "Query did not return a single integer result."

  -- Close the database connection
  close conn

import Core
import Db.Migrations

main :: IO ()
main = do
  migrate
  runDiabloServer

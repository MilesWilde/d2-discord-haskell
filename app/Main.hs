import Core
import Db.GameData.GDDbCommands
import Db.Migrations

main :: IO ()
main = do
  migrate
  runDiabloServer

-- classStats

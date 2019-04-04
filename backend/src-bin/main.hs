import Backend
import Frontend
import Obelisk.Backend
import System.Environment (getArgs)

main :: IO ()
main = do
  runCheck <- not . null . filter (== "check-deployment") <$> getArgs
  if runCheck
     then checkDeployment
     else runBackend backend frontend

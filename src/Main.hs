-- Compare different lists of mail addresses.
-- This file contains the main of the project which should provide a dynamic
-- command line interface with the user

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import System.Console.Haskeline
import Control.Monad.IO.Class

import Commands (commandMap)
import Types
import Tools

-- Command error string
errorstr = init $ unlines 
                ["ERROR: command not known."
                ,"Type 'help' to get the list of valid commands"
                ]


-- Function that parses the command and call the appropriate functions according
-- to the request
parse :: EStatus -> Maybe String -> IO EStatus
parse status Nothing = return status
parse status (Just string) =
                  case Map.lookup (stringHead command) commandMap of
                  Just f -> f status options
                  Nothing -> putStrLn errorstr >> return status
                  where (command, options) = splitAt 1 $ words string
                          

welcome = unlines ["Welcome!"
                  ,"For a list of available commands type 'help'" 
                  ,"To exit type 'exit'"]


command :: EStatus -> InputT IO ()
command status = getInputLine "~> " >>= command'
                 where
                   command' :: Maybe String -> InputT IO ()
                   command' mstr = liftIO (parse status mstr) >>= command

               
main = putStr welcome >>
       runInputT defaultSettings (command (toEStatus Map.empty))

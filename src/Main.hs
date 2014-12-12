-- Compare different lists of mail addresses.
-- This file contains the main of the project which should provide a dynamic
-- command line interface with the user

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

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
parse :: EStatus -> String -> IO EStatus
parse status string = if null string
                      then return status
                      else case Map.lookup (stringHead command) commandMap of
                           Just f -> f status options
                           Nothing -> putStrLn errorstr >> return status
                           where (command, options) = splitAt 1 $ words string
                          

welcome = unlines ["Welcome!"
                  ,"For a list of available commands type 'help'" 
                  ,"To exit type 'exit'"]

main = putStr welcome >>
       return (toEStatus Map.empty) >>=
       command 
       where command status = putStr "~> " >>
                              hFlush stdout >>
                              getLine >>= 
                              (parse status) >>=
                              command

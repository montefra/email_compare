module Commands (commandMap)
where 

import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Exit
import System.Directory

import Types
import CSVmail
import Tools

-- Map that list the commands key and the corresponding function. It is the
-- public interface
commandMap :: CommandMap
commandMap = Map.fromList [("help", help)
                          ,("exit", \_ _ -> putStrLn "goodbye" >> exitSuccess)
                          ,("listtags", listtags) 
                          ,("load", load)
                          ,("diff", diff)
                          ,("printall", printall)
                          ,("print", printtag)
                          ]

-- Help string. `unlines` add a trailing \n and `init` removes it
help :: EStatus -> [String] -> IO EStatus
help status _ = putStr helpStr >> return status
                where helpStr = unlines 
                                ["Welcome to `emails_compare`."
                                ,"I guess that you want to know how I work, right?"
                                ,"Available commands:"
                                ,"    + help: print this help"
                                ,"    + load tag filename: load all the email addresses from"
                                ,"        file *name* and attach them to *tag* keyword."
                                ,"        Existing tags will be silently overwritten."
                                ,"    + diff tag1 tag2: show the emails that are in *tag1*"
                                ,"        but not in *tag2*"
                                ,"    + listtags: list the tags already present"
                                ,"    + printall: print the full dictionary"
                                ,"    + print tag: print the content of tag"
                                ,"    + exit: exit the program"
                                ]


-- Lists the tags in the status dictionary
listtags :: EStatus -> [String] -> IO EStatus
listtags status _ = putStrLn (message keys) >> return status
                    where keys = unlines $ map (\(n, k) -> (show n) ++ ": " ++ k) numKey
                          numKey = zip [1 ..] $ Map.keys $ fromEStatus status
                          message "" = "No tag found"
                          message xs = "The available tags are:\n" ++ init xs


-- read the input file and store its emails into the status
load :: EStatus -> [String] -> IO EStatus
load status (tag:filename:[]) = loadifFile status tag filename
load status (tag:filename:_) = putStrLn msg >> load status (tag:filename:[])
                               where msg = unwords ["The command is 'load tag filename'."
                                                   ,"Anything else is ignored"
                                                   ]
load status _ = putStrLn "The command is too short. The correct one is 'load tag filename'"
                >> return status

-- if the file exists, read it and update the status. Otherwise print a warning
-- and return the original status
loadifFile :: EStatus -> Tag -> FilePath -> IO EStatus
loadifFile s t fn = doesFileExist fn >>= loadIfExist
                    where loadIfExist b = if b
                                          then loadFile s t fn
                                          else putStrLn ("'" ++ fn ++ "' does not exists")
                                              >> return s
-- Now we are sure that the file exists: go ahead
loadFile :: EStatus -> Tag -> FilePath -> IO EStatus
loadFile s t fn = pure (esInsert t) <*> emails <*> (return s)
                  where emails = readFile fn >>=
                                (\s' -> return (csv2eMails s')) 


-- get the difference between the sets attached to two tags 
diff :: EStatus -> [String] -> IO EStatus
diff status (tag1:tag2:[]) = putStrLn (pureDiff status tag1 tag2) >> return status
diff status (tag1:tag2:_) = putStrLn msg >> diff status (tag1:tag2:[])
                               where msg = unwords ["The command is 'diff tag1 tag2'."
                                                   ,"Anything else is ignored"
                                                   ]
diff status _ = putStrLn "The command is too short. The correct one is 'diff tag1 tag2'"
                >> return status

pureDiff :: EStatus -> Tag -> Tag -> String
pureDiff status tag1 tag2 = case doDiff status tag1 tag2 of
                            Just set -> pplist (Set.toList set) ""
                            Nothing -> "One of the tags does not exist"

doDiff :: EStatus -> Tag -> Tag -> Maybe (Set.Set EMail)
doDiff status t1 t2 = pure Set.difference <*> mapLookup t1 <*> mapLookup t2
                      where mapLookup k = Map.lookup k $ fromEStatus status

-- print the whole dictionary
printall :: EStatus -> [String] -> IO EStatus
printall status [] = putStrLn msg >> return status
                     where ppmsg = ppEStatus status
                           msg = if null ppmsg 
                                 then "No entries found"
                                 else ppmsg

-- print the content of a single tag
printtag :: EStatus -> [String] -> IO EStatus
printtag status (tag:[]) = putStrLn ppstr >> return status
                           where ppstr = case Map.lookup tag (fromEStatus status) of
                                         Just set -> pplist (Set.toList set) ""
                                         Nothing -> "The tag '" ++ tag ++ "' does not exist"
printtag status (tag:_) = putStrLn msg >> printtag status (tag:[])
                          where msg = unwords ["The command is 'printtag tag'."
                                              ,"Anything else is ignored"
                                              ]
printtag status _ = putStrLn "The command is too short. The correct one is 'printtag tag'"
                    >> return status



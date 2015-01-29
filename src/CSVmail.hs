module CSVmail (csv2eMails)
where

-- Compare various email lists
import qualified Data.Set as S
import Control.Applicative
import Text.Regex.Posix

import Types (EMail)
import Tools (safeTail)

type CSV = String

isCommaOrNewLine :: Char -> Bool
isCommaOrNewLine c = c == ',' || c == '\n'

-- split the input string into a list of string assuming that the input is the
-- content of a CSV file
splitCSV :: CSV -> [String]
splitCSV [] = []
splitCSV xs = let (xhead, tail') = break isCommaOrNewLine xs
                  xtail = safeTail tail'  -- remove the leading comma or new line at
              in xhead:(splitCSV xtail)

-- extract what looks like an email out of a list of strings
getEMails :: [String] -> [EMail]
getEMails = filter ('@' `elem`)

charmatch = "[a-zA-Z0-9_.-]"
regexp = charmatch ++ "*@" ++ charmatch ++ "*" 

-- get the emails from the content of a CSV 
csv2eMails :: CSV -> S.Set EMail
-- csv2eMails = S.fromList . getEMails . splitCSV
csv2eMails csv = S.fromList $ concat (csv =~ regexp)


-- file names
newCSV = "../Famiglie_bambini_17_11_2014.csv" :: FilePath
itCSV = "../IT_email_e_compiti_Verteiler.csv" :: FilePath

main = let newMails = fmap csv2eMails $ readFile newCSV
           itMails = fmap csv2eMails $ readFile itCSV
           newVsIt = pure S.difference <*> newMails <*> itMails
           itVsNew = pure S.difference <*> itMails <*> newMails
        in fmap show newVsIt >>= putStrLn >>
           print "++++++" >>
           fmap show itVsNew >>= putStrLn 

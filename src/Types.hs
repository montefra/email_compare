module Types (EMail
             ,EStatus(fromEStatus)
             ,toEStatus
             ,esInsert
             ,Tag
             ,CommandMap
             )
where

import qualified Data.Map as Map
import qualified Data.Set as Set

type EMail = String
type Tag = String

-- status of the program and functions to wrap map commands
data EStatus = EStatus {fromEStatus :: Map.Map String (Set.Set EMail)}

toEStatus = EStatus

esInsert :: Tag -> (Set.Set EMail) -> EStatus -> EStatus
esInsert key value status = toEStatus $ Map.insert key value $ fromEStatus status

-- the commands are stored into a map. This simply redefine the type for
-- simplicity
type CommandMap = Map.Map Tag (EStatus -> [String] -> IO EStatus)

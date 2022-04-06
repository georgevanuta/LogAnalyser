{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


            --------Parsing a Message -> LogMessage --------
--Transforms list of words([String]) to a sentence(String)
wordsToString :: [String] -> String
wordsToString xs = foldr (\wrd accStr -> wrd ++ " " ++ accStr) [] xs

--The previous function adds a whitespace at the end, this function is made to remove it
remLastChr :: String -> String 
remLastChr [] = []
remLastChr str = take (length str - 1) str

--Composes the previous two functions
word :: [String] -> String 
word = remLastChr . wordsToString

--Parses a message to a LogMessage
parseMessage :: String -> LogMessage
parseMessage str
    | head str == 'I' = LogMessage Info (read ((!!1)(words str)) :: Int) (word (drop 2 (words str)))
    | head str == 'W' = LogMessage Warning (read ((!!1)(words str)) :: Int) (word (drop 2 (words str)))
    | head str == 'E' = LogMessage (Error (read ((!!1)(words str)) :: Int)) (read ((!!2)(words str)) :: Int) (word (drop 3 (words str)))
    | otherwise = Unknown str

--Parses a whole file to a list of LogMessage's
parse :: String -> [LogMessage]
parse str = foldr (\currStr accLogs -> (parseMessage currStr):accLogs) [] strs
    where strs = lines str

            --------Sorting the logs--------
--Extracts the timeStamp from a LogMessage
timeStamp :: LogMessage -> TimeStamp  
timeStamp (Unknown _) = error unknownLogError --no timeStamp -> Unknown log type -> thow exception 
timeStamp (LogMessage _ time _) = time

--Insert in binary search tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTr = msgTr
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node lftTree m rgtTree)
    | (timeStamp msg) < (timeStamp m) = Node (insert msg lftTree) m rgtTree
    | otherwise = Node lftTree m (insert msg rgtTree)

            --------Exercise 3--------
--Build a binary search tree based on a list of LogMessages
build :: [LogMessage] -> MessageTree
build logs = foldr (\currLog accTree -> insert currLog accTree) Leaf logs

            --------Exercise 4--------
--Inorder traversal to sort and filter the list of LogMessages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lftTree msg rgtTree) = (inOrder lftTree) ++ [msg] ++ (inOrder rgtTree)

            --------Exercise 5--------
--If a log is of error type and has severity > 50 -> True / otherwise -> False
isSevere :: LogMessage -> Bool 
isSevere (LogMessage (Error severity) _ _)
    | severity >= 50 = True 
    | otherwise = False
isSevere _ = False 

--Extracts the message from a LogMessage
message :: LogMessage -> String 
message (LogMessage _ _ msg) = msg
message _ = error unknownLogError   --Unknown log type -> thow exception

--Macro function to sort 
sortLogs :: [LogMessage] -> [LogMessage]
sortLogs = inOrder . build

--Filters and orders logs for severe errors
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map message (filter isSevere sortedLogs)
    where sortedLogs = sortLogs logs
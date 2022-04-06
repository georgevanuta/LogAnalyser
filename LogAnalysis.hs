{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


            --------Exercise 1--------
wordsToString :: [String] -> String
wordsToString xs = foldr (\wrd accStr -> wrd ++ " " ++ accStr) [] xs

remLastChr :: String -> String 
remLastChr [] = []
remLastChr str = take (length str - 1) str

word :: [String] -> String 
word = remLastChr . wordsToString

parseMessage :: String -> LogMessage
parseMessage str
    | head str == 'I' = LogMessage Info (read ((!!1)(words str)) :: Int) (word (drop 2 (words str)))
    | head str == 'W' = LogMessage Warning (read ((!!1)(words str)) :: Int) (word (drop 2 (words str)))
    | head str == 'E' = LogMessage (Error (read ((!!1)(words str)) :: Int)) (read ((!!2)(words str)) :: Int) (word (drop 3 (words str)))
    | otherwise = Unknown str

parse :: String -> [LogMessage]
parse str = foldr (\currStr accLogs -> (parseMessage currStr):accLogs) [] strs
    where strs = lines str

            --------Exercise 2--------
timeStamp :: LogMessage -> Int 
timeStamp (Unknown _) = error unknownLogError  
timeStamp (LogMessage _ time _) = time

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTr = msgTr
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node lftTree m rgtTree)
    | (timeStamp msg) < (timeStamp m) = Node (insert msg lftTree) m rgtTree
    | otherwise = Node lftTree m (insert msg rgtTree)

            --------Exercise 3--------
build :: [LogMessage] -> MessageTree
build logs = foldr (\currLog accTree -> insert currLog accTree) Leaf logs

            --------Exercise 4--------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lftTree msg rgtTree) = (inOrder lftTree) ++ [msg] ++ (inOrder rgtTree)

            --------Exercise 5--------
isSevere :: LogMessage -> Bool 
isSevere (LogMessage (Error severity) _ _)
    | severity >= 50 = True 
    | otherwise = False
isSevere _ = False 

message :: LogMessage -> String 
message (LogMessage _ _ msg) = msg
message _ = error unknownLogError

sortLogs :: [LogMessage] -> [LogMessage]
sortLogs = inOrder . build

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map message (filter isSevere sortedLogs)
    where sortedLogs = sortLogs logs
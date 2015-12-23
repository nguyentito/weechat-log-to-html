import Data.Char
import Data.Function
import Data.List
-- TODO: use Text instead of linked lists of chars

type WeechatLog = [WeechatLine]
data WeechatLine = WeechatLine { wlDate :: String
                               , wlTime :: String
                               , wlNick :: String
                               , wlMsg :: String }
-- TODO: specific handling of join/part/network messages

main = (printHTML . parseWeechatLog) =<< getContents

parseWeechatLog :: String -> [WeechatLine]
parseWeechatLog = map parseWeechatLine . lines
  where parseWeechatLine l =
          let [date, time, nick] = take 3 . words $ l
              msg = drop (length (unwords [date, time, nick]) + 1) l
          in WeechatLine date time nick msg

printHTML :: [WeechatLine] -> IO ()
printHTML log = do header <- readFile "head.html"
                   putStrLn header
                   putStrLn "<body>"
                   mapM_ printDay days
                   putStrLn "</body>"
                   putStrLn "</html>"
  where days = groupBy ((==) `on` wlDate) log
        printDay ls = do
          putStrLn $ "<h3>" ++ wlDate (head ls) ++ "</h3>"
          putStrLn $ "<table>"
          mapM_ printRow ls
          putStrLn $ "</table>"
        printRow l = do
          putStr $ "<tr><td>" ++ wlTime l ++ "</td>"
          putStr $ "<td>" ++ wlNick l ++ "</td>"
          putStrLn $ "<td>" ++ escape (wlMsg l) ++ "</td></tr>"

escape = concat . map entity
  where entity '<' = "&lt;"
        entity '>' = "&gt;"
        entity c = [c]
  


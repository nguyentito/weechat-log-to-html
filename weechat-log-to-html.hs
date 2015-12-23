import Data.Char
import Data.Function
import Data.List
import qualified Data.Set as Set
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
  where allNicks = Set.fromList . map (dropWhile sigil . wlNick) $ log
        days = groupBy ((==) `on` wlDate) log
        printDay ls = do
          putStrLn $ "<h3>" ++ wlDate (head ls) ++ "</h3>"
          putStrLn $ "<table>"
          mapM_ printRow $ zip (WeechatLine "" "" "" "" : ls) ls
          putStrLn $ "</table>"
        printRow (prevRow, curRow) = do
          putStr $ "<tr><td>" ++ wlTime curRow ++ "</td>"
          putStr $ "<td class=\"" ++ ac ++ "\">" ++ nick ++ "</td>"
          putStrLn $ "<td>" ++ (colorhl allNicks . escape $ wlMsg curRow) ++ "</td></tr>"
          where prevNick = wlNick prevRow
                curNick = wlNick curRow
                nick | specialNick curNick = curNick
                     | prevNick == curNick = "↳"
                     | otherwise = curNick
                ac = nickClass curNick

specialNick = (`elem` ["-->","<--","--","*"])

nickClass "-->" = "nc-join"
nickClass "<--" = "nc-quit"
nickClass "--" = "nc-network"
nickClass "*" = "nc-slashme"
nickClass str = ("nc-color-" ++) . hash . dropWhile sigil $ str

sigil = (`elem` "@%+")
-- Weechat default nick hash function = sum of unicode values
hash = show . (`mod` (length colors)) . sum . map ord

colors = ["cyan","magenta","green","brown","lightblue","default",
          "lightcyan","lightmagenta","lightgreen","blue"]

colorhl allNicks msg
  | firstWord == "" = msg
  | last firstWord == ':' && nick `Set.member` allNicks =
      sigils ++ "<span class=\"nc-color-" ++ hash nick ++ "\">" ++ nick ++ "</span>:" ++ rest
  | otherwise = msg
  where (firstWord, rest) = span (not . isSpace) msg
        (sigils, nick') = span sigil firstWord
        nick = init nick'

escape = concat . map entity
  where entity '<' = "&lt;"
        entity '>' = "&gt;"
        entity c = [c]
  


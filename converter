import Data.List.Split
import Data.Char
import Data.List
import System.Directory

-- Header
header :: [String]
header = ["Date","Type","Exchange","Base amount","Base currency","Quote amount","Quote currency","Fee","Fee currency","Costs/Proceeds","Costs/Proceeds currency","Sync Holdings","Sent/Received from","Sent to","Notes"]

-- Add symbols here that does not match between Delta and Binance
symbolConvert :: String -> String
symbolConvert "IOTA" = "MIOTA"
symbolConvert x = x
   
-- Creates list of files that starts with "OrderHistory" 
getOrderFiles :: [String] -> [String]
getOrderFiles [] = []
getOrderFiles (x:xs) | "OrderHistory" `isPrefixOf` x = ("./Old/" ++ x) : getOrderFiles xs
getOrderFiles (x:xs) = getOrderFiles xs

--Main function to call to convert trades
convert_trades :: String -> String
convert_trades x = unlines $ map (intercalate ",")  ([header] ++ convTxs (readTrades x))

--Creates a list of lists from the .csv data
readTrades :: String -> [[String]]
readTrades x = removeTrash (map (splitOn ",") (lines x))

--Removes unnecessary lines
removeTrash :: [[String]] -> [[String]]
removeTrash [] = []
removeTrash (("Date(UTC)":xs):xxs) = removeTrash xxs
removeTrash ((x:("Date(UTC)":xs)):xxs) = removeTrash xxs
removeTrash (x:xs) = x:removeTrash xs

--Converts the transactions
convTxs :: [[String]] -> [[String]]
convTxs [] = []
convTxs ((x:xs):xxs) | null x = convTxs xxs
convTxs ((x:xs):xxs) = transaction : convTxs rest where
    (ts,rest) = transactions [] xxs
    [pair,tType,_,_,_,_,_,_] = xs
    [date,_,bAmount,qAmount,fc,_,_,_] = head ts
    (bCoin,qCoin) = splitPair pair
    (fee, fCoin) = splitFee [] fc
    transaction = [date,tType,"Binance",bAmount,bCoin,qAmount,qCoin,fee,fCoin,"","","1","","",""]

-- Returns the transactions that belong together
transactions :: [[String]] -> [[String]] -> ([[String]],[[String]])
transactions ts [] = (ts, [])
transactions ts ((x:xs):xxs) | not $ null x = (ts,(x:xs):xxs)
transactions ts ((_:xs):xxs) = transactions (ts ++ [xs]) xxs

--Splits the trading pairs
splitPair :: String -> (String,String)
splitPair x = splitR $ reverse x

splitR :: String -> (String,String)
splitR ('C':'T':'B':xs) = (reverse xs, "BTC")
splitR ('H':'T':'E':xs) = (reverse xs, "ETH")
splitR ('B':'N':'B':xs) = (reverse xs, "BNB")
splitR ('T':'D':'S':'U':xs) = (reverse xs, "USDT")

--Splits the fee and fee symbols
splitFee :: String -> String -> (String,String)
splitFee s (x:xs) | isDigit x = splitFee (s++[x]) xs
                  | x == '.' = splitFee (s++[x]) xs
                  | otherwise = (s, (x:xs))

--Deposits and withdrawals
convert_deposits :: String -> String
convert_deposits x = unlines $ map (intercalate ",")  ([header] ++ convDW "DEPOSIT" (readDW x))

convert_withdrawals :: String -> String
convert_withdrawals x = unlines $ map (intercalate ",")  ([header] ++ convDW "WITHDRAW" (readDW x))

readDW :: String -> [[String]]
readDW x = (map (splitOn ",") (tail $ lines x))

convDW :: String -> [[String]] -> [[String]]
convDW t [] = []
convDW t (x:xs) = transaction:convDW t xs where
    [date,symbol,bAmount,fee,_,txid,_,_,_] = x
    from = case t of "DEPOSIT" -> "OTHER"
                     "WITHDRAW" -> "Binance"
    to = case t of "DEPOSIT" -> "Binance"
                   "WITHDRAW"  -> "OTHER"
    coin = symbolConvert symbol
    transaction = [date,t,"Binance",bAmount,coin,"","",fee,coin,"","","",from,to,txid]
----------------------------------------------------

main :: IO ()
main = do
    putStr "Available commands: \nconvert_trades <filename>\nconvert_deposits <filename>\nconvert_withdrawals <filename>\n\n"
    command <- getLine
    let cmd = words command
    if head cmd == "convert_trades" then do
        csvData <- readFile ("./" ++ last cmd)
        writeFile "newTrades.csv" $ convert_trades csvData
        putStrLn "\nFile converted\n"
        main
    else if head cmd == "convert_deposits" then do
        csvData <- readFile ("./" ++ last cmd)
        writeFile "newDeposits.csv" $ convert_deposits csvData
        putStrLn "\nFile converted\n"
        main
    else if head cmd == "convert_withdrawals" then do
        csvData <- readFile ("./" ++ last cmd)
        writeFile "newWithdrawals.csv" $ convert_withdrawals csvData
        putStrLn "\nFile converted\n"
        main
    else do
        putStrLn "Error, wrong command. Try again\n"
        main

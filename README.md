This Haskell code converts Binance history to a format Delta can read. Supports order history, deposit history and withdrawal history. 

How to use it:
1. Download the history you want to convert from Binance.
2. Open the .xlsx file in Excel and save it as CSV. Do not save it as CSV UTF-8 or another format.
3. Put the .csv file in the same folder as the converter. 
4. Run the converter and use one of the available commands, convert_trades, convert_deposits or convert_withdrawals along with the file name.
   E.g "convert_trades OrderHistory1.csv"

Will add the ability to convert several files at once in the future. 

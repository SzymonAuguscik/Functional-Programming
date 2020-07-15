import Data.Map
import System.Exit (exitSuccess)

{-
    Instrukcja obslugi:
        Program nalezy uruchomic za pomoca polecenia main.
        Nastepnie uzytkownik zostanie poproszony o podanie danych do logowania.
        Po weryfikacji mozliwa bedzie jedna z 4 operacji:
            - wplacenie pieniedzy
            - wyplacenie pieniedzy
            - zmiana uzytkownika
            - wyjscie
        Po pierwszych dwoch operacjach, jesli zostana zakonczone sukcesem, pojawi
        sie informacja o stanie konta uzytkownika.
-}

--data
data ATM = ATM {cashCount::(Map Int Int)} deriving (Show, Read, Eq)
data User = User {name::String, pin::Int} deriving (Show, Read, Eq, Ord)

--init fields
bills = [200, 100, 50, 20, 10]
minBill = minimum bills

--print
printUser::Map User Int -> String -> Int -> IO()
printUser uMap name pin =
    if (member user uMap) then
        (putStrLn (name ++ "'s bank account: " ++ (show (uMap ! user))))
    else
        error "Incorrect user name or password" where
    user = User name pin

--check/verify functions
verifyUser::Map User Int -> String -> Int -> Bool
verifyUser uMap name pin = member user uMap where
    user = User name pin

checkWithdraw::Map User Int -> String -> Int -> Int -> Bool
checkWithdraw uMap name pin money = money <= (uMap ! user) where
    user = User name pin

checkDivisibility::Int -> Int -> Bool
checkDivisibility _ 0 = error "Division by 0 is forbidden!"
checkDivisibility a b = a `mod` b == 0

checkATM::ATM -> Bool
checkATM atm = minInATM >= 0 where
    bills = Prelude.map (\x -> snd x) (toList (cashCount atm))
    minInATM = Prelude.foldl min 100 bills

--money functions
depositMoney::Map User Int -> String -> Int -> Int -> Map User Int
depositMoney uMap name pin money = update f user uMap where 
    user = (User name pin)
    f x = Just (x + money)

withdrawMoney::Map User Int -> String -> Int -> Int -> Map User Int
withdrawMoney uMap name pin money = update f user uMap where
    user = (User name pin)
    f x = Just (x - money)

moneyToBills::Int -> Int -> [(Int, Int)]
moneyToBills 0 _ = []
moneyToBills money acc =
    (if (amount > 0) then
        [(currBill, amount)]
    else
    [])
    ++ (moneyToBills (money-currBill*amount) (acc+1)) where
    currBill = bills !! acc
    amount = money `div` currBill

changeATM::ATM -> [(Int, Int)] -> Int -> ATM
changeATM atm list 0 = ATM (unionWith (+) (cashCount atm) (fromList list))
changeATM atm list 1 = ATM (unionWith (-) (cashCount atm) (fromList list))

--main & menu
menu::String -> Int -> Map User Int -> ATM -> IO()
menu name pin uMap atm =
    if (not (verifyUser uMap name pin)) then do
        putStrLn "Incorrect user name or pin! Try again"
        putStr "Name: "
        name <- getLine
        putStr "PIN: "
        pin1 <- getLine
        let pin = read pin1::Int
        menu name pin uMap atm
    else do
        putStrLn "1. Deposit money"
        putStrLn "2. Withdraw money"
        putStrLn "3. Change user"
        putStrLn "4. Exit"
        
        choice <- getLine
        
        if (choice == "3") then do
            putStr "Name: "
            name <- getLine
            putStr "PIN: "
            pin1 <- getLine
            let pin = read pin1::Int
            menu name pin uMap atm
        else
            return ()

        if (not (elem choice ["1","2","3"])) then
            exitSuccess
        else
            putStr "Write money amount: "

        money1 <- getLine
        let money = read money1::Int
        
        if (checkDivisibility money minBill) then
            if (not (checkWithdraw uMap name pin money) && choice == "2") then do
                putStrLn "Not enough money to withdraw!"
                menu name pin uMap atm
            else do
                let newMainATM = case choice of "1" -> (changeATM atm (moneyToBills money 0) 0); "2" -> (changeATM atm (moneyToBills money 0) 1)
                if (checkATM newMainATM) then do
                    let newUserMap = case choice of "1" -> (depositMoney uMap name pin money); "2" -> (withdrawMoney uMap name pin money)
                    printUser newUserMap name pin
                    menu name pin newUserMap newMainATM
                else do
                    putStrLn "Not enough bills to end transaction! Aborting..."
                    menu name pin uMap atm
        else do
            putStrLn (show money ++ " is not a multiple of " ++ show minBill ++ "!")
            menu name pin uMap atm

main::IO()
main = do
    putStr "Name: "
    name <- getLine
    putStr "PIN: "
    pin1 <- getLine
    let pin = read pin1::Int
    let mainATM = ATM (fromList (Prelude.map (\x -> (x,100)) bills))
    let userMap = fromList [((User "Abelard" 1234), 150::Int), ((User "Boris" 9931), 2000), ((User "Celine" 6512), 5000), ((User "Dany" 3149), 2400)]
    menu name pin userMap mainATM

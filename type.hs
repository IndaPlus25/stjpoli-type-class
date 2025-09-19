data Card = Card Int String

exist :: Card -> Bool --Can be rewritten into a clean boolean
exist (Card val col) 
    | val > 14 = False
    | val < 0 = False
    | not (elem col tab) = False 
    | otherwise = True
    where tab = ["Hearts", "Diamonds", "Clubs", "Spades"]

show' :: Card -> String
show' c@(Card val col) -- The card is saved in c
    | not (exist c) = "Not a Card" -- Check if card exists
    | otherwise = "The " ++ show val ++ " of " ++ col 

eq' :: (Card, Card) -> Bool
eq' (Card val1 col1,Card val2 col2)
    | val1 /= val2 = False
    | col1 /= col2 = False
    | otherwise = True

ord' :: Card -> Card -> Bool
ord' (Card val1 _) (Card val2 _) = val1 < val2

sort' :: [Card] -> [Card] --Quick sort for the cards
sort' [] = []
sort' (c@(Card val _):cs) = sort' [x | x <- cs, ord' x c] ++ [c] ++ sort' [x | x <- cs, not (ord' x c)] 

main :: IO()
main = do
    print(show' (Card 3 "Clubs"))
    print(show' (Card 35 "Clubs"))
    print(sort' [(Card 6 "Clubs"), (Card 3 "Clubs"), (Card 2 "Clubs")])




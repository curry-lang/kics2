-- Testing Read and Show instances for various types

import Test.EasyCheck

showOfRead :: (Eq a, Read a, Show a) => a -> Prop
showOfRead x = read (show x) -=- x

showOfReadBool :: Bool -> Prop
showOfReadBool = showOfRead

showOfReadInt :: Int -> Prop
showOfReadInt = showOfRead

showOfReadChar :: Char -> Prop
showOfReadChar c = ord c >= 0 ==> showOfRead c

showOfReadOrdering :: Ordering -> Prop
showOfReadOrdering = showOfRead

showOfReadString :: String -> Prop
showOfReadString s = all (\c -> ord c >= 0) s ==> showOfRead s

showOfReadMaybeInt :: Maybe Int -> Prop
showOfReadMaybeInt = showOfRead

showOfReadEitherCharBool :: Either Int Bool -> Prop
showOfReadEitherCharBool = showOfRead

showOfReadPairIntBool :: (Int,Bool) -> Prop
showOfReadPairIntBool = showOfRead

showOfReadMaybeOrdering :: Maybe Ordering -> Prop
showOfReadMaybeOrdering = showOfRead

showOfReadListInt :: [Int] -> Prop
showOfReadListInt = showOfRead


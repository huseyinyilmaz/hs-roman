import Test.QuickCheck
import Data.Roman

newtype SmallInt = SmallInt Int deriving (Eq,Show)

instance Arbitrary SmallInt where
  arbitrary = do val <- choose (1, 999)
                 return $ SmallInt val


-- When we do following conversion we should get the same value we started
-- Int -> Roman -> Str -> Roman -> Int
prop_str_convert :: SmallInt -> Bool
prop_str_convert (SmallInt i) = i == (romanToInt.read.show.intToRoman) i

main :: IO()
main = quickCheckWith stdArgs { maxSuccess = 50000 } prop_str_convert

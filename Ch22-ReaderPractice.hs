
module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- x1 :: Maybe (Integer, Integer)
-- x1 = traverse xs 

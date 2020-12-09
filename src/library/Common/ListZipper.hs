module Common.ListZipper
where

import Protolude hiding (toList)

data ListZipper a = ListZipper [a] a [a] deriving Show

fromList :: [a] -> Maybe (ListZipper a)
fromList [] = Nothing
fromList (x:xs) = Just $ ListZipper [] x xs

stepRight :: ListZipper a -> Maybe (ListZipper a)
stepRight (ListZipper bs v (x:xs)) = Just $ ListZipper (v:bs) x xs
stepRight (ListZipper _ _ []) = Nothing

stepLeft :: ListZipper a -> Maybe (ListZipper a)
stepLeft (ListZipper (b:bs) v xs) = Just $ ListZipper bs b (v:xs)
stepLeft (ListZipper [] _ _) = Nothing

toList :: ListZipper a -> [a]
toList lz@(ListZipper _ v xs) = case stepLeft lz of
  Nothing -> v:xs
  Just lz' -> toList lz'

focus :: ListZipper a -> a
focus (ListZipper _ v _) = v

update :: (a -> a) -> ListZipper a -> ListZipper a
update f (ListZipper bs v xs) = ListZipper bs (f v) xs

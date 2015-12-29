module ListUtil (..) where


import List


get : Int -> List a -> Maybe a
get index list =
  List.head (List.drop index list)


{-
  If `list` has an element at `index`, then sets it to `value`.
  Otherwise, returns `list` unmodified.
-} 
set : Int -> a -> List a -> List a
set index value list =
  List.indexedMap (\i x -> if i == index then value else x) list


{-
  If `list` has an element at `index`, then passes it to `f` and replaces it with the result.
  Otherwise, returns `list` unmodified.
-}
update : Int -> (a -> a) -> List a -> List a
update index f list =
  List.indexedMap (\i x -> if i == index then f(x) else x) list


{- If `list` has en element at `index`, then removes it.
   Otherwise, returns `list` unmodified.
-}
remove : Int -> List a -> List a
remove index list =
  list
    |> List.indexedMap (,)
    |> List.filterMap (\(i, x) -> if i == index then Nothing else Just x)

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

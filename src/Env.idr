module Env

public export
Env : Type
Env = List (String, Double)

public export
envLookup : String -> Env -> Maybe Double
envLookup _ []              = Nothing
envLookup k ((k', v) :: xs) = if k == k' then Just v else envLookup k xs

public export
envInsert : String -> Double -> Env -> Env
envInsert k v []                    = [(k, v)]
envInsert k v ((k', v') :: rest) =
  if k == k' then (k, v) :: rest
              else (k', v') :: envInsert k v rest

public export
envToList : Env -> List (String, Double)
envToList = id

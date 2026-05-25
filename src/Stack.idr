module Stack

public export
data Stack : Nat -> Type where
  Empty : Stack 0
  Push  : Double -> Stack n -> Stack (S n)

public export
push : Double -> Stack n -> Stack (S n)
push = Push

public export
pop : Stack (S n) -> (Double, Stack n)
pop (Push x s) = (x, s)

public export
peek : Stack (S n) -> Double
peek (Push x _) = x

public export
dup : Stack (S n) -> Stack (S (S n))
dup (Push x s) = Push x (Push x s)

public export
swap : Stack (S (S n)) -> Stack (S (S n))
swap (Push x (Push y s)) = Push y (Push x s)

public export
binOp : (Double -> Double -> Double) -> Stack (S (S n)) -> Stack (S n)
binOp f (Push y (Push x s)) = Push (f x y) s

public export
toList : Stack n -> List Double
toList Empty      = []
toList (Push x s) = x :: toList s

public export
depth : Stack n -> Nat
depth Empty      = 0
depth (Push _ s) = S (depth s)

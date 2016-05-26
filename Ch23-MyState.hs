
newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \a -> (a, a)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) x = let (_, s) = sa x
                    in s

eval :: State s a -> s -> a
eval (State sa) x = let (a, _) = sa x
                    in a

-- not quite perfect yet

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)

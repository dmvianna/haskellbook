{-# LANGUAGE InstanceSigs #-}

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

-- We need instances here

instance Functor (State s) where
  fmap :: (a -> b)
       -> State s a
       -> State s b
  fmap f (State g) = State $ \s -> let (a, b) = g s
                                   in (f a, b)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)
  (<*>) :: State s (a -> b)
        -> State s a
        -> State s b
  (State f) <*> (State g) = State $ \s -> let (fab, s') = f s
                                              (a, s'') = g s'
                                          in (fab a, s'')

instance Monad (State s) where
  return = pure
  (>>=) :: State s a
        -> (a -> State s b)
        -> State s b
  (State f) >>= g = State $ \s -> let a = fst $ f s
                                      ms = runState $ g a
                                  in ms s
  (>>) :: State s a
       -> State s b
       -> State s b
  State f >> State g = State $ \s -> let (_, s') = f s
                                     in g s'

--

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)

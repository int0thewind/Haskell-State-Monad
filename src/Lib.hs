module Lib where

newtype State s a = StateConstructor { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f state = StateConstructor (\v -> let (x, v') = runState state v
                                           in  (f x, v'))

instance Applicative (State s) where
    pure x = StateConstructor (\s -> (x, s))
    -- Both the appling function and state are State
    f <*> state = StateConstructor (\v -> let (a1, s1) = runState state v
                                              (a2, s2) = runState f s1
                                          in  (a2 a1, s2))

instance Monad (State s) where
    return = pure
    state >>= f = StateConstructor (\v -> let (a, s') = runState state v
                                          in  runState (f a) s')

get :: p -> State a a
get x = StateConstructor (\x -> (x, x))

put :: s -> State s ()
put x = StateConstructor (\s -> ((), x))

inc :: Num a => a -> a
inc = (+ 1)

double :: Num a => a -> a
double = (* 2)

incState :: Num a => State a a
incState = StateConstructor (\s -> (double s, inc s))

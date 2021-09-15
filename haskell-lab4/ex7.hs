module Stack (module Stack) where
--module Stack (Stack(MkStack), empty, isEmpty, push, top, pop) where
--module Stack (Stack(..), isEmpty, push, pop) where
--module Stack (Stack, push, pop) where

empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)

newtype Stack a = MkStack [a] deriving Show

empty = MkStack []
isEmpty (MkStack s) = null s
push x (MkStack s) = MkStack (x:s)
top (MkStack s) = head s
pop (MkStack (s:ss)) = (s,MkStack ss)
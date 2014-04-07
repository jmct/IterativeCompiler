module Fresh where

import Control.Applicative

data Fresh a = Fresh { runFresh :: String -> Int -> (Int, a) }

instance Functor Fresh where
  fmap f (Fresh k) = Fresh $ \s i -> let (i', a) = k s i
                                     in (i', f a)

instance Applicative Fresh where
  pure a = Fresh $ \s i -> (i, a)
  (Fresh f) <*> (Fresh g) = Fresh $ \s i -> let (i', f')  = f s i
                                                (i'', a)  = g s i'
                                            in (i'', f' a)

instance Monad Fresh where
  return = pure
  (Fresh h) >>= f = Fresh $ \s i -> let (i1, s1) = h s i
                                        (Fresh g) = f s1
                                        in g s i1

{- Naylor's bind implementation
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)
 -}

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ show i))

scfresh :: Fresh String
scfresh = Fresh (\s i -> (i+1, s ++ " " ++ show i))

ignore :: String -> Fresh a -> Fresh a
ignore str (Fresh f) = Fresh $ \_ i -> f str i


labelInt :: Fresh String
labelInt = Fresh (\s i -> (i, s ++ show i))

labelNoInt = Fresh $ \s i -> (i, s)

labelAppendInt :: Fresh a -> Fresh a
labelAppendInt (Fresh f) = Fresh $ \s i -> f (s ++ show i) i

labelNewCount :: Fresh String
labelNewCount = Fresh $ \s i -> (1, s ++ ": 0")

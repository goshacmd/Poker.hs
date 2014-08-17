{-# LANGUAGE
    ConstraintKinds, DataKinds, TypeOperators, TypeFamilies, FlexibleInstances,
    MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Data.Tuple.Pack where

import GHC.Exts

class Pack t a | t -> a where
  packN :: [a] -> t
  unpackN :: t -> [a]

instance AllSame [a, b] => Pack (a, b) a where
  packN = pack2
  unpackN = unpack2

instance AllSame [a, b, c] => Pack (a, b, c) a where
  packN = pack3
  unpackN = unpack3

instance AllSame [a, b, c, d] => Pack (a, b, c, d) a where
  packN = pack4
  unpackN = unpack4

instance AllSame [a, b, c, d, e] => Pack (a, b, c, d, e) a where
  packN = pack5
  unpackN = unpack5

pack2 (a:b:_) = (a, b)
pack3 (a:b:c:_) = (a, b, c)
pack4 (a:b:c:d:_) = (a, b, c, d)
pack5 (a:b:c:d:e:_) = (a, b, c, d, e)

unpack2 (a, b) = [a, b]
unpack3 (a, b, c) = [a, b, c]
unpack4 (a, b, c, d) = [a, b, c, d]
unpack5 (a, b, c, d, e) = [a, b, c, d, e]

type family AllSame (xs :: [*]) :: Constraint where
  AllSame '[]  = ()
  AllSame '[a] = ()
  AllSame (a ': b ': xs) = (a ~ b, AllSame (b ': xs))

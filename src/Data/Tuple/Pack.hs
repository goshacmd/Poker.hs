module Data.Tuple.Pack where

class Pack t a | t -> a where
  packN :: [a] -> t
  unpackN :: t -> [a]

instance Pack (a, a) a where
  packN = pack2
  unpackN = unpack2

instance Pack (a, a, a) a where
  packN = pack3
  unpackN = unpack3

instance Pack (a, a, a, a) a where
  packN = pack4
  unpackN = unpack4

instance Pack (a, a, a, a, a) a where
  packN = pack5
  unpackN = unpack5

pack2 :: [a] -> (a, a)
pack2 (a:b:_) = (a, b)

unpack2 :: (a, a) -> [a]
unpack2 (a, b) = [a, b]

pack3 :: [a] -> (a, a, a)
pack3 (a:b:c:_) = (a, b, c)

unpack3 :: (a, a, a) -> [a]
unpack3 (a, b, c) = [a, b, c]

pack4 :: [a] -> (a, a, a, a)
pack4 (a:b:c:d:_) = (a, b, c, d)

unpack4 :: (a, a, a, a) -> [a]
unpack4 (a, b, c, d) = [a, b, c, d]

pack5 :: [a] -> (a, a, a, a, a)
pack5 (a:b:c:d:e:_) = (a, b, c, d, e)

unpack5 :: (a, a, a, a, a) -> [a]
unpack5 (a, b, c, d, e) = [a, b, c, d, e]

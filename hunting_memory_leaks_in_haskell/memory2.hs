{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
import System.Environment
-- import Data.NumInstances
import Data.List (foldl')
import qualified Control.Foldl as L
import Control.Applicative

-- call mean
main = do
    [d] <- map read `fmap` getArgs
    print $ mean5 [1..d]

-- mean1
mean1 :: [Double] -> Double
mean1 xs = sum xs / fromIntegral (length xs)

-- try running with 1e7












-- mean2
mean2 :: [Double] -> Double
mean2 xs = uncurry (/) $ foldl' (\(!x1,!x2) y -> (x1 + y, x2 + 1)) (0,0) xs

-- try running with 1e7
-- try running without -O2











-- mean3
-- numinstances
mean3 :: [Double] -> Double
mean3 = uncurry (/) . sum . map (,1)

-- try running with 1e7





lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
lift2 fa fb (a,b) = (fa a, fb b)

-- Equivalently, lift2 = (***)

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = let !a1 = a+a'
                        !b2 = b+b'
                    in (a1,b2)
  (a,b) - (a',b') = (a-a',b-b')
  (a,b) * (a',b') = (a*a',b*b')
  negate = lift2 negate negate
  abs    = lift2 abs abs
  signum = lift2 signum signum



-- mean4
-- disable numinstances
--
-- enable:
-- lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
-- lift2 fa fb (a,b) = (fa a, fb b)

-- -- Equivalently, lift2 = (***)

-- instance (Num a, Num b) => Num (a,b) where
--   fromInteger n   = (fromInteger n, fromInteger n)
--   (a,b) + (a',b') = let a1 = a+a'
--                         b1 = b+b'
--                     in a1 `seq` b1 `seq` (a1,b1)
--   (a,b) - (a',b') = (a-a',b-b')
--   (a,b) * (a',b') = (a*a',b*b')
--   negate = lift2 negate negate
--   abs    = lift2 abs abs
--   signum = lift2 signum signum
mean4 :: [Double] -> Double
mean4 = uncurry (/) . sum . zip [1..]

-- try running with 1e7
-- try running with and without -O2








-- lazy list (strict spine/value)
-- show examples
data List a = Nil | Cons !a !(List a) deriving Show

fromList :: [a] -> List a
fromList = foldr Cons Nil











-- streams
mean5 :: [Double] -> Double
mean5 = L.fold $ (/) <$> L.sum <*> L.genericLength

-- try running with ie7
-- with and without -O2

-- show pipe/conduit examples

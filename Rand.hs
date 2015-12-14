module Rand (Gen, oneOf, oneInRange, eitherGen, Random, genSeed, genIO) where

import Control.Monad(ap,liftM)
import System.Random(randomR, StdGen, Random, mkStdGen, newStdGen)

newtype Gen a = G (StdGen -> (a,StdGen))

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
  pure x = G (\g -> (x,g))
  (<*>)  = ap

instance Monad Gen where
  G m >>= k = G (\g -> let (x,g1) = m g
                           G m1   = k x
                       in m1 g1)


-- | Choose one of the options uniformly.  The list should be non-empty
oneOf :: [a] -> Gen a
oneOf [] = error "OneOf []"
oneOf xs = G $ \g -> let (i,g1) = randomR rng g
                     in (xs !! i, g1)
  where
  rng = (0, length xs - 1)

-- | Chose a value uniformly from the given closed interval.
oneInRange :: Random a => (a,a) -> Gen a
oneInRange rng = G (randomR rng)


-- | Use the first generator a certain percantage of the time.
eitherGen :: Int -> Gen a -> Gen a -> Gen a
eitherGen d g1 g2 =
  do x <- oneInRange (0,99)
     if x < d then g1 else g2


genSeed :: Int -> Gen a -> a
genSeed seed (G m) = fst (m (mkStdGen seed))

genIO :: Gen a -> IO a
genIO (G m) = (fst . m) <$> newStdGen




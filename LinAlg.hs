{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, ScopedTypeVariables #-}
module LinAlg
  ( V
  , type (~>)
  , type (+)
  , vec, mat, apply, (#), add
  , vmap, mmap
  , spectralRadius
  , unV, fromMatrix
  ) where

import qualified Numeric.LinearAlgebra as LA
import Data.List(sortBy)
import Data.Complex(Complex(..))
import Data.Maybe(listToMaybe)
import Control.Monad(guard)
import GHC.TypeLits
import Data.Proxy


-- | A vector with that many entries.
newtype V (x :: Nat)              = V (LA.Vector Double)

-- | A matrix with `x` columns and `y` rows.
newtype (x :: Nat) ~> (y :: Nat)  = M (LA.Matrix Double)

infixr 2 ~>
infixr 2 #

instance Show (V x) where
  showsPrec p (V x) = showsPrec p x

instance Show (x ~> y) where
  showsPrec p (M x) = showsPrec p x

-- | Make a vector from a list.
-- If there are not enought elements in the list, the rest are 0.
-- If there are too many elements in the list, the rest are ignored.
vec :: forall n. KnownNat n => [ Double ] -> V n
vec xs = V (LA.fromList (take len (xs ++ repeat 0)))
  where len = fromInteger (natVal (Proxy :: Proxy n))

-- | Elements in row order (i.e. first `y` elements is the 1st row, etc)
mat :: forall x y. (KnownNat x, KnownNat y) => [ Double ] -> x ~> y
mat xs = M ( (rs LA.>< cs) (take (rs * cs) (xs ++ repeat 0)))
  where cs = fromInteger (natVal (Proxy :: Proxy x))
        rs = fromInteger (natVal (Proxy :: Proxy y))

-- | Apply a matrix to a vector.
apply :: (x ~> y) -> V x -> V y
apply (M f) (V x) = V (f LA.#> x)

-- | Add to vectors together.
add :: V x -> V x -> V x
add (V x) (V y) = V (LA.add x y)

-- | Concatenate two vectors.
(#) :: V x -> V y -> V (x + y)
V x # V y = V (LA.vjoin [x,y])

-- | Aplly a function to all elements of a vector.
vmap :: (Double -> Double) -> V x -> V x
vmap f (V x) = V (LA.cmap f x)

-- | Apply a function to all elements of a matrix.
mmap :: (Double -> Double) -> (x ~> y) -> (x ~> y)
mmap f (M x) = M (LA.cmap f x)

-- | Compute the spectral radius of a matrix.
spectralRadius :: (x ~> y) -> Maybe Double
spectralRadius (M m) = listToMaybe (sortBy (flip compare) realVs)
  where
  realVs = [ r | r :+ i <- LA.toList (LA.eigenvalues m), i <= 0.00001 ]

-- | Underlying representation of a vector.
unV :: V n -> LA.Vector Double
unV (V x) = x

-- | Make a matrix from the underlying representation of a matrix.
fromMatrix ::
  forall x y. (KnownNat x, KnownNat y) => LA.Matrix Double -> Maybe (x ~> y)
fromMatrix m = do guard (cs == LA.cols m && rs == LA.rows m)
                  return (M m)
  where cs = fromInteger (natVal (Proxy :: Proxy x))
        rs = fromInteger (natVal (Proxy :: Proxy y))



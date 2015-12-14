{-# LANGUAGE DataKinds, TypeOperators, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
import LinAlg
import Rand
import GHC.TypeLits(KnownNat,natVal)
import Data.Proxy
import Data.List(unfoldr)
import Control.Monad(replicateM)
import Data.Maybe(fromMaybe)
import qualified Numeric.LinearAlgebra as LA


data Net x i y = Net
  { internal    :: V i
  , output      :: V y

  , inputFun    :: x      ~> i
  , internalFun :: i      ~> i
  , outputFun   :: x + i  ~> y
  , backFun     :: y      ~> i

  , transfer    :: Double -> Double
  , transferOut :: Double -> Double
  }


{-# INLINE step #-}
step :: V x -> Maybe (V y) -> Net x i y -> Net x i y
step xs mbTeach Net { .. } = Net { internal = newInternal
                                 , output = newOutput, .. }
  where
  -- 1.6
  newInternal = vmap transfer
              $ foldr1 add [ apply inputFun    xs
                           , apply internalFun internal
                           , apply backFun     (fromMaybe output mbTeach)
                           ]

  -- 1.7
  newOutput   = vmap transferOut
              $ apply outputFun (xs # newInternal)


train :: (KnownNat y, KnownNat (x+i)) =>
         Int          {- ^ Skip this many of the first steps -} ->
         [(V x, V y)] {- ^ Training input/output examples -}    ->
         Net x i y    {- ^ Net whose outputs need training -}   ->
         Net x i y
train skip tData n0 = case fromMatrix learnedOutput of
                        Just o  -> n0 { outputFun = o }
                        Nothing -> error "bug with matrix dimensions in train"
  where
  (xs,ys0) = unzip tData

  learnedOutput = LA.tr' (LA.pinv steps LA.<> teachOut)

  -- (inp - skip) rows, each: y
  teachOut = LA.fromRows
           $ map (unV . vmap atanh)
           $ drop skip ys0

  -- Make training steps; initial "previous" ouputs is 0
  -- (inp - skip) rows, each: (x + (i + y))
  steps    = LA.fromRows
           $ drop skip
           $ unfoldr trainStep (xs, vec [] : ys0, n0)

  trainStep (inps,prevOuts,n) =
    case (inps,prevOuts) of
      -- inp (n)
      (is : inps', ds : prevOuts') ->
        Just $ let newN = step is (Just ds) n
                   snap = unV (is # (internal newN # output n))
               in (snap, (inps', prevOuts', newN))
      _ -> Nothing





--------------------------------------------------------------------------------



-- | A random value in the interval -1 to 1 inclusive.
-- The density is the chance that the number will be non-zero, as a percent.
-- For example, 15 means that 15% of the values are non-zero.
randomCts :: Int -> Gen Double
randomCts d = eitherGen d (oneInRange (-1,1)) (return 0)

-- | A random value that will be either -1 or 1.
-- The density is the chance that the number will be non-zero, as a percent.
-- For example, 15 means that 15% of the values are non-zero.
randomDsc :: Int -> Gen Double
randomDsc d = eitherGen d (oneOf [-1,1]) (return 0)

-- | Generate a random matrix, using the given generator.
randMatrix :: forall x y. (KnownNat x, KnownNat y) => Gen Double -> Gen (x ~> y)
randMatrix gen = mat <$> replicateM (fromInteger els) gen
  where els = natVal (Proxy :: Proxy x) * natVal (Proxy :: Proxy y)

-- | Generate random internal connection, with the desired spectra radius.
randInternal :: KnownNat i => Double        {- ^ Desired spectral radius -} ->
                              Gen Double -> {- ^ How to initialize matrix -}
                              Gen (i ~> i)
randInternal r gen = pick
  where
  pick = do m <- randMatrix gen
            case spectralRadius m of
              Just x | abs x > 0.00001  -> return (mmap (\v -> v * r / x) m)
              _                         -> pick


data NetSetup = NetSetup
  { genInput        :: Gen Double
  , genInternal     :: Gen Double
  , genBack         :: Gen Double
  , useRadius       :: Double
  , useTransfer     :: Double -> Double
  , useTransferOut  :: Double -> Double
  }

-- | Generate a dynamic reservoir.
dynReservor ::
  (KnownNat x, KnownNat i, KnownNat y, KnownNat (x+i)) =>
    NetSetup -> Gen (Net x i y)
dynReservor NetSetup { .. } =
  do inputFun    <- randMatrix genInput
     internalFun <- randInternal useRadius genInternal
     backFun     <- randMatrix genBack
     return Net { internal    = vec []
                , output      = vec []
                , outputFun   = mat []  -- This is what we need to train
                , transfer    = useTransfer
                , transferOut = useTransferOut
                , ..
                }


test :: IO ()
test = do m <- genIO (randInternal 0.8 (randomCts 20))
          print (spectralRadius (m :: 8 ~> 8))




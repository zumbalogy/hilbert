-- https://hackage.haskell.org/package/fractals-0.1.0.0/docs/src/Data-SpaceFillingCurve-Hilbert-Integer-Internal.html#hilbert
-- |
-- Module      : Data.SpaceFillingCurve.Hilbert.Integer.Internal
-- Copyright   : (c) 2015 Stephen Dekker <steve.dekk@gmail.com>
-- License     : BSD3
--
-- Maintainer  : steve.dekk@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This modules contains the implementation of Butz's Hilbert curve
-- encoding algorithm. Of these, the hilbert and hilbertInverse function
-- are exposed through the "Data.SpaceFillingCurve.Hilbert.Integer" module.

module Data.SpaceFillingCurve.Hilbert.Integer.Internal (
        -- * Encoding and decoding the Hilbert curve
        hilbert,          -- :: (Bits a, Bits b, Num b) => Int -> [a] -> b
        hilbertInverse,   -- :: (Bits a, Bits b) => Int -> Int -> a -> [b]

        -- * Internal helper functions for the Hilbert transformations
        bitAt,            -- :: (Bits a, Bits b) => a -> Int -> b
        trailingSetBits,  -- :: (Bits a, Num b) => a -> b
        mask,             -- :: Num a => Int -> a
        rotR,             -- :: (Num a, Bits a) => Int -> a -> Int -> a
        rotL,             -- :: (Num a, Bits a) => Int -> a -> Int -> a
        grayCode,         -- :: Bits a => a -> a
        grayCodeInverse,  -- :: Bits a => a -> a
        entryPoint,       -- :: (Num a, Bits a) => a -> a
        direction,        -- :: (Num a, Bits a) => Int -> a -> Int
        transform,        -- :: (Num a, Bits a) => Int -> a -> Int -> a -> a
        transformInverse, -- :: (Num a, Bits a) => Int -> a -> Int -> a -> a
        pivot             -- :: (Bits a, Bits b) => Int -> a -> [Int] -> [b]
  ) where

import           Data.Bits (Bits, bit, clearBit, setBit, shiftL, shiftR,
                            testBit, xor, zeroBits, (.&.), (.|.))

------------------------------------------
-- Encoding and decoding the Hilbert curve

-- The variable names (symbols) used in Hamilton's paper are reproduced in
-- the bodies of these two functions. These names are overly terse, but are
-- useful when comparing the implementation side-by-side with Hamilton's
-- report. More descriptive names were chosen for the helper functions and
-- the sequences described in the original paper.

-- | Given the number of bits required to represent the largest value in
-- the given input list (which represents a point in an N-dimensional
-- Cartesian space), returns the Hilbert index of the point.

hilbert :: (Bits a, Bits b, Num b) => Int -> [a] -> b
hilbert precision ps = hilbertIndex
  where (_, _, hilbertIndex) = foldr step start [0..precision-1]
        n = length ps
        start = (zeroBits, zeroBits, zeroBits)
        step i (e, d, h) = seq e' $ seq d' $ seq h' (e', d', h')
          where l  = foldr (\x acc -> (acc `shiftL` 1) .|. (x `bitAt` i))
                     zeroBits (reverse ps)
                t  = transform n e d l
                w  = grayCodeInverse t
                h' = (h `shiftL` n) .|. w
                e' = e `xor` rotL n (entryPoint w) (d+1)
                d' = (d + direction n w + 1) `mod` n

-- | Given the number of bits required to represent the largest value in
-- the output vector, the number of dimensions in the output space and the
-- Hilbert index of the output point, returns a list of values representing
-- the point in Cartesian space.

hilbertInverse :: (Num a, Bits a, Bits b) => Int -> Int -> a -> [b]
hilbertInverse precision n hilbertIndex = points
  where (_, _, points) = foldr step start [0..precision-1]
        start = (0::Integer, zeroBits, replicate n zeroBits)
        step i (e, d, ps) = seq e' $ seq d' $ seq ps' (e', d', ps')
          where w   = foldr (\x acc -> (acc `shiftL` 1) .|.
                               (hilbertIndex `bitAt` (i*n + x)))
                      zeroBits [0..n-1]
                t   = grayCode w
                l   = transformInverse n e d t
                ps' = zipWith (.|.) ps (pivot i l (reverse [0..n-1]))
                e'  = e `xor` rotL n (entryPoint w) (d+1)
                d'  = (d + direction n w + 1) `mod` n

------------------------------------------------------------
-- Internal helper functions for the Hilbert transformations

-- | Returns the value of the given bit in the source bit string. Note that
-- if the bit was set, the returned value will be of the output type with
-- only the first bit set.

bitAt :: (Bits a, Bits b) => a -> Int -> b
bitAt x i = if x `testBit` i then bit 0 else zeroBits

-- | Counts the number of trailing set bits in the given bit string.

trailingSetBits :: (Bits a, Num b) => a -> b
trailingSetBits i = go i 0
  where go j acc = if not (testBit j 0)
                     then acc
                     else go (j `shiftR` 1) (acc+1)

-- | Creates a bit mask extending the range of bits from [0, 'width' - 1].

mask :: Num a => Int -> a
mask width = 2^width - 2 + fromIntegral (signum width)

-- | Performs a windowed right rotate by 'i' within a window from bit 0 to
-- bit 'width' on a number 'x'.

rotR :: (Num a, Bits a) => Int -> a -> Int -> a
rotR width x i = trunc ((trunc x `shiftR` s) .|. (x `shiftL` (width - s)))
  where s = i `mod` width
        trunc = (.&.) (mask width)

-- | Performs a windowed left rotate by 'i' within a window from bit 0 to
-- bit 'width' on a number 'x'.

rotL :: (Num a, Bits a) => Int -> a -> Int -> a
rotL width x i = trunc ((x `shiftL` s) .|. (trunc x `shiftR` (width - s)))
  where s = i `mod` width
        trunc = (.&.) (mask width)

-- | Returns the 'i'-th binary-reflected Gray code.

grayCode :: Bits a => a -> a
grayCode i = i `xor` (i `shiftR` 1)

-- | Returns the enumeration index of a given binary-reflected Gray code,
-- inverting the Gray code transform.

grayCodeInverse :: Bits a => a -> a
grayCodeInverse g = go g g 1
  where go i acc j = if acc == zeroBits
                       then i
                       else go (i `xor` (g `shiftR` j))
                               (acc `shiftR` 1) (j+1)

-- | Returns the 'i'-th element in the sequence of entry points.

entryPoint :: (Num a, Bits a) => a -> a
entryPoint i | signum i == -1 = error "Input must be positive"
             | i == zeroBits  = zeroBits
             | otherwise      = grayCode ((i-1) `clearBit` 0)

-- | Given the dimensionality of the Hilbert curve and an index 'i',
-- returns the 'i'-th element in the sequence of directions.

direction :: (Num a, Bits a) => Int -> a -> Int
direction n i | signum i == -1 = error "Input must be positive"
              | i == zeroBits  = zeroBits
              | testBit i 0    = trailingSetBits i `mod` n
              | otherwise      = trailingSetBits(i-1) `mod` n

-- | Given a dimensionality, an entry point, a direction and a Gray code
-- representing a canonical, unrotated sub-hypercube path we wish to
-- transform, returns the path rotated so that it is correctly oriented
-- within its quadrant.

transform :: (Num a, Bits a) => Int -> a -> Int -> a -> a
transform n e d l = rotR n (l `xor` e) (d+1)

-- | Given a dimensionality, an entry point, a direction and the Gray code
-- representing the rotated sub-hypercube path in a particular quadrant,
-- returns the path rotated and transformed back into its canonical form.

transformInverse :: (Num a, Bits a) => Int -> a -> Int -> a -> a
transformInverse n e d l = e `xor` rotL n l (d+1)

-- | Given a position, 'i', a bit-array 'l' and a list of positions to
-- test, returns a list containing either the value (2^'i') or 0 depending
-- on whether the bits in 'l' at the positions in the input list are set or
-- not.

pivot :: (Bits a, Bits b) => Int -> a -> [Int] -> [b]
pivot i l = map (\j -> setBitIf (testBit l j) zeroBits i)
  where setBitIf True  = setBit
        setBitIf False = const

{-# Language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- code structure based on:
-- https://www.cs.indiana.edu/cgi-pub/sabry/cnf1.hs

import Control.Applicative
import Control.Monad.RWS
import Data.Foldable (foldrM)
import Data.List (unfoldr, dropWhileEnd, partition, sortBy, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, maybe, isNothing)
import Data.Numbers.Primes
import qualified Data.Set as Set
import System.Environment (getProgName, getArgs)
import System.IO
import Test.HUnit
import Test.QuickCheck

-----
-- Settings
-----

cnfDebug = False -- show debug comments in the dimacs output
maxThree = False -- make sure every clause has at most three literals
minThree = False -- make sure every clause has at least three literals
                 -- TODO move setting from the gates to clause rendering


-----
-- Symbolic evaluation
-----

-- A Bit is either a fixed boolean value or a variable (identified by
-- an integer)
data Bit = Val {getVal :: Bool}
         | Var {getVar :: Int}
         deriving (Eq)

instance Show Bit where
  show (Val False) = "F"
  show (Val True) = "T"
  show (Var v) = show v

isVar, isVal :: Bit -> Bool
isVar (Var _) = True
isVar _       = False
isVal (Val _) = True
isVal _       = False

toBool :: Bit -> Maybe Bool
toBool (Var _) = Nothing
toBool (Val x) = Just x

zero, one :: Bit
zero = Val False
one  = Val True

-- negate bit
neg :: Bit -> Bit
neg (Val b) = Val (not b)
neg (Var v) = Var (-v)

prop_doubleNeg :: Bit -> Bool
prop_doubleNeg b = neg (neg b) == b

instance Arbitrary Bit where
  arbitrary = frequency [(15, liftM Val arbitrary)
                        ,(1, liftM Var (getNonZero <$> arbitrary))]
  shrink (Val b) = [Val b' | b' <- shrink b]
  shrink (Var v) = [Var v' | v' <- shrink v]

nextVar :: SymEval Bit
nextVar = state (\v -> (Var v, v+1))

data Clause = Clause { getClause :: [Bit] }
            | Comment { getComment :: String }
            deriving (Eq, Show)

isClause, isComment :: Clause -> Bool
isClause (Clause _) = True
isClause _ = False
isComment (Comment _) = True
isComment _ = False

instance Arbitrary Clause where
  arbitrary = frequency [( 1, liftM Comment arbitrary)
                        ,(15, liftM Clause arbitrary)]
  shrink (Clause c) = [Clause c' | c' <- shrink c]
  shrink (Comment c) = [Comment c' | c' <- shrink c]

newtype Cnf = Cnf { getCnf :: [Clause] }
            deriving (Eq, Show, Arbitrary, Semigroup, Monoid)

removeValues :: Cnf -> Cnf
removeValues = Cnf . map removeFalse . filter removeTrue . getCnf
  where removeTrue (Clause c) = Val True `notElem` c
        removeTrue _ = True
        removeFalse (Clause c) = Clause $ filter (/= Val False) c
        removeFalse c = c

removeComments :: Cnf -> Cnf
removeComments = Cnf . filter isClause . getCnf

toDimacsClauses :: Cnf -> String
toDimacsClauses = unlines . map toDimacsClause . getCnf . removeValues
  where toDimacsClause (Clause c) = unwords . (++["0"]) $ map (show . getVar) c
        toDimacsClause (Comment c) = "c " ++ c

countClauses :: Cnf -> Int
countClauses = length . filter isClause . getCnf . removeValues

type Desc = Cnf

toDimacs :: Int -> Desc -> String
toDimacs n c = header ++ toDimacsClauses c
  where header = "p cnf " ++ show n ++ " " ++ show (countClauses c) ++ "\n"

seToDimacs :: SymEval a -> String
seToDimacs se = toDimacs (n-1) d
  where (_, n, d) = runRWS se (cnfDebug, maxThree, minThree) 1

debugCircuit :: Show a => SymEval a -> String
debugCircuit se = toDimacsClauses d ++ show r
  where (r, n, d) = runRWS se (True, maxThree, minThree) 1

addClauses :: MonadWriter Desc m => [[Bit]] -> m ()
addClauses = tell . Cnf . map Clause

addClause :: MonadWriter Desc m => [Bit] -> m ()
addClause c = addClauses [c]

debugComment :: (MonadReader (Bool, Bool, Bool) m, MonadWriter Desc m) => String -> m ()
debugComment c = do
  (debug, _, _) <- ask
  tell $ if debug then Cnf [Comment c] else mempty

addComment :: MonadWriter Desc m => String -> m ()
addComment c = tell $ Cnf [Comment c]

-- Symbolic evaluation of the gate/circuit.  The writer logs
-- information about the circuit, while the State can be used
-- to introduce new variables.
type SymEval x = RWS (Bool, Bool, Bool) Desc Int x

eval :: SymEval x -> x
eval se = fst $ evalRWS se (cnfDebug, maxThree, minThree) 1


-----
-- Miscelaneous helper functions
-----

-- fold a non-empty list as a tree
treeFoldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
treeFoldM1 _  []  = error "empty list in treeFoldM1"
treeFoldM1 _  [x] = return x
treeFoldM1 fn xs  = sequence (pairs xs) >>= treeFoldM1 fn
  where pairs []       = []
        pairs [x]      = [return x]
        pairs (x:y:ys) = (fn x y) : (pairs ys)

-- fold a list as a tree
treeFoldM :: Monad m => (a -> a -> m a) -> a -> [a] -> m a
treeFoldM _  base [] = return base
treeFoldM fn _    xs = treeFoldM1 fn xs

-- digits of a number
digits :: Integral n => n -> n -> [n]
digits 1 n = replicate (fromIntegral n) 1
digits radix 0 = []
digits radix n = let (q, r) = quotRem n radix
                  in r : digits radix q

-- inverse of digits
undigits :: Integral n => n -> [n] -> n
undigits 1 = fromIntegral . length 
undigits radix = go 1
  where go _ []     = 0
        go b (d:ds) = b * d + go (b * radix) ds

prop_undigits :: Integral n => (Positive n) -> (NonNegative n) -> Bool
prop_undigits (Positive radix) (NonNegative n) =
  n == undigits radix (digits radix n)

evenOdd :: [a] -> ([a], [a])
evenOdd []       = ([], [])
evenOdd [x]      = ([x], [])
evenOdd (e:o:xs) = let (es, os) = evenOdd xs in (e:es, o:os)

-----
-- Elementary gates
-----

notGate :: Bit -> SymEval Bit
notGate = return . neg

nandGate :: Bit -> Bit -> SymEval Bit
nandGate (Var x)     (Val y) = nandGate (Val y) (Var x)
nandGate (Val False) y       = return (Val True)
nandGate (Val True)  y       = return (neg y)
nandGate x           y       = do
  z <- nextVar
  (_, _, minThree) <- ask
  if minThree
    then addClauses [[x, z, z]
                    ,[y, z, z]
                    ,[neg x, neg y, neg z]]
    else addClauses [[x, z]
                    ,[y, z]
                    ,[neg x, neg y, neg z]]
  return z

norGate :: Bit -> Bit -> SymEval Bit
norGate x y = nandGate (neg x) (neg y) >>= notGate

andGate :: Bit -> Bit -> SymEval Bit
andGate x y = nandGate x y >>= notGate

orGate :: Bit -> Bit -> SymEval Bit
orGate x y = nandGate (neg x) (neg y)

xorGate :: Bit -> Bit -> SymEval Bit
xorGate (Var x)     (Val y) = xorGate (Val y) (Var x)
xorGate (Val False) y       = return y
xorGate (Val True)  y       = return $ neg y
xorGate x           y       = do
  z <- nextVar
  addClauses [[    x,     y, neg z]
             ,[    x, neg y,     z]
             ,[neg x,     y,     z]
             ,[neg x, neg y, neg z]]
  return z

xnorGate :: Bit -> Bit -> SymEval Bit
xnorGate x y = xorGate x y >>= notGate

-- multiplexer: depending on s, output x or y
muxGate :: Bit -> Bit -> Bit -> SymEval Bit
muxGate (Val False) x _ = return x
muxGate (Val True)  _ y = return y
muxGate s           x y = do
  z <- nextVar
  addClauses [[    s,     x, neg z]
             ,[    s, neg x,     z]
             ,[neg s,     y, neg z]
             ,[neg s, neg y,     z]]
  return z

-- n-bit multiplexer: depending on s, output xs or ys
muxNGate :: Bit -> [Bit] -> [Bit] -> SymEval [Bit]
muxNGate (Val False) xs _  = return xs
muxNGate (Val True)  _  ys = return ys
muxNGate s xs ys = zipWithM (muxGate s) xs ys

-- n-bit or-gate: check if any of the bits is True
orNGate :: [Bit] -> SymEval Bit
orNGate = treeFoldM orGate zero

-- n-bit or-gate without output
assertOrNGate :: [Bit] -> SymEval ()
assertOrNGate xs = do
  (_, maxThree, minThree) <- ask
  if maxThree || minThree
    then orNGate xs >> return ()
    else addClauses [xs]

-- n-bit and-gate: check if all bits are True
andNGate :: [Bit] -> SymEval Bit
andNGate = treeFoldM andGate one
       
-- n-bit eq-gate: check if all bits in xs are equal to all ys
eqNGate :: [Bit] -> [Bit] -> SymEval Bit
eqNGate xs ys = do
  debugComment $ show xs ++ " == " ++ show ys
  zs <- zipWithM xnorGate xs ys
  andNGate zs

assertBoolGate :: Bool -> Bit -> SymEval ()
assertBoolGate x (Val y) = unless (x == y) $ error "assertBoolGate"
assertBoolGate x y = do
  (_, _, minThree) <- ask
  if minThree then addClause [z, z, z] else addClause [z]
  where z = if x then y else neg y

assertEqGate :: Bit -> Bit -> SymEval ()
assertEqGate (Val x) y       = assertBoolGate x y
assertEqGate x       (Val y) = assertBoolGate y x
assertEqGate x           y   = do
  debugComment $ show x ++ " == " ++ show y
  (_, _, minThree) <- ask
  if minThree
    then addClauses [[x, neg y, neg y]
                    ,[neg x, y, y]]
    else addClauses [[x, neg y]
                    ,[neg x, y]]

assertEqNGate :: [Bit] -> [Bit] -> SymEval ()
assertEqNGate = zipWithM_ assertEqGate

-----
-- Adders
-----

-- 1-bit half adder
halfAdd :: Bit -> Bit -> SymEval (Bit, Bit)
halfAdd x y = do
  debugComment $ "Start HA: " ++ show (x, y)
  s <- xorGate x y
  c <- andGate x y
  debugComment $ "End HA: " ++ show (x, y) ++ " -> " ++ show (s, c)
  return (s, c)

-- test the entire truth table
test_halfAdd = TestList
  [ TestCase . assertEqual "ha-ff" (zero, zero) . eval $ halfAdd zero zero
  , TestCase . assertEqual "ha-ft" (one , zero) . eval $ halfAdd zero one 
  , TestCase . assertEqual "ha-tf" (one , zero) . eval $ halfAdd one  zero
  , TestCase . assertEqual "ha-tt" (zero, one ) . eval $ halfAdd one  one 
  ]

fullAdd :: Bit -> Bit -> Bit -> SymEval (Bit, Bit)

fullAdd (Val x) (Val y) (Val ci) = return (Val s, Val co)
  where s = (x /= y) /= ci
        co = if ci then x || y else x && y

fullAdd (Val x) (Val y) ci = fullAdd ci (Val x) (Val y)
fullAdd (Val x) y ci = fullAdd y (Val x) ci
fullAdd x (Val y) (Val ci) = return (s, co)
  where s = if y == ci then x else neg x
        co = if y == ci then (Val y) else s

fullAdd x (Val y) ci = fullAdd x ci (Val y)
fullAdd x y (Val False) = halfAdd x y
fullAdd x y (Val True)  = do
  s <- xnorGate x y
  c <- orGate x y
  return (s, c)

fullAdd x y ci = do
  (_, maxThree, _) <- ask
  debugComment $ "Start FA: " ++ show (x, y, ci)
  if maxThree
    then do (s1, c1) <- halfAdd x y
            (s2, c2) <- halfAdd s1 ci
            co <- orGate c1 c2
            return (s2, co)
    else do s <- nextVar
            addClauses [[neg x, neg y, neg ci,     s]
                       ,[neg x, neg y,     ci, neg s]
                       ,[neg x,     y, neg ci, neg s]
                       ,[neg x,     y,     ci,     s]
                       ,[    x, neg y, neg ci, neg s]
                       ,[    x, neg y,     ci,     s]
                       ,[    x,     y, neg ci,     s]

                       ,[    x,     y,     ci, neg s]]
            co <- nextVar
            addClauses [[neg x, neg y,             co]
                       ,[neg x,        neg ci,     co]
                       ,[    x,     y,         neg co]
                       ,[    x,            ci, neg co]
                       ,[       neg y, neg ci,     co]
                       ,[           y,     ci, neg co]]
            debugComment $ "End FA: " ++ show (x, y, ci) ++ " -> " ++ show (s, co)
            return (s, co)

-- test the entire truth table
test_fullAdd = TestList
  [ TestCase . assertEqual "fa-fff" (zero, zero) . eval $ fullAdd zero zero zero
  , TestCase . assertEqual "fa-fft" (one , zero) . eval $ fullAdd zero zero one 
  , TestCase . assertEqual "fa-ftf" (one , zero) . eval $ fullAdd zero one  zero
  , TestCase . assertEqual "fa-ftt" (zero, one ) . eval $ fullAdd zero one  one 
  , TestCase . assertEqual "fa-tff" (one , zero) . eval $ fullAdd one  zero zero
  , TestCase . assertEqual "fa-tft" (zero, one ) . eval $ fullAdd one  zero one 
  , TestCase . assertEqual "fa-ttf" (zero, one ) . eval $ fullAdd one  one  zero
  , TestCase . assertEqual "fa-ttt" (one , one ) . eval $ fullAdd one  one  one 
  ]


-----
-- Binary representation of numbers
-----

newtype UInt = UInt { getUInt :: [Bit] }
  deriving (Eq, Arbitrary)

instance Show UInt where
  show u@(UInt xs) = "UInt " ++
    (case uint2Int u of
      Just n  -> show n ++ " " 
      Nothing -> ""
    ) ++ show xs

newtype SInt = SInt { getSInt :: [Bit] }
  deriving (Eq)

instance Show SInt where
  show s@(SInt xs) = "SInt " ++
    (case sint2Int s of
       Just n  -> show n ++ " " 
       Nothing -> ""
    ) ++ show xs

-- TODO instance Arbitrary SInt with a non-empty list

getSign :: SInt -> Bit
getSign (SInt xs) = last xs

uintZero :: UInt
uintZero = UInt []

uintOne :: UInt
uintOne = UInt [one]

sintZero :: SInt
sintZero = SInt [zero]

sintOne :: SInt
sintOne = SInt [one, zero]

int2UInt :: Integral n => n -> UInt
int2UInt = UInt . map (Val . (==1)) . digits 2

uint2Int :: Integral n => UInt -> Maybe n
uint2Int (UInt xs) = do
  ys <- mapM toBool xs
  return . undigits 2 . map (fromIntegral . fromEnum) $ ys

prop_uint :: Integral n => (NonNegative n) -> Bool
prop_uint (NonNegative n) = n == (fromJust . uint2Int . int2UInt $ n)

int2SInt :: Integral n => n -> SInt
int2SInt n
  | n < 0     = let lgn = ceiling . logBase 2 . fromIntegral $ (-n)
                    (UInt xs) = int2UInt $ 2^(lgn+1) + n
                 in SInt xs
  | otherwise = let (UInt xs) = int2UInt n
                 in SInt (xs ++ [zero])

sint2Int :: Integral n => SInt -> Maybe n
sint2Int (SInt xs) = do
  sign <- toBool (last xs)
  y <- uint2Int (UInt (init xs))
  return $ if sign then y - 2^(length xs - 1) else y

prop_sint :: Integral n => n -> Bool
prop_sint n = n == (fromJust . sint2Int . int2SInt $ n)

padUInt :: Int -> UInt -> UInt
padUInt n (UInt xs) = UInt (xs ++ replicate (n - length xs) zero)

shrinkUInt :: UInt -> UInt
shrinkUInt (UInt xs) = UInt . dropWhileEnd (== zero) $ xs

prop_padShrinkUInt :: (NonNegative Int) -> UInt -> Bool
prop_padShrinkUInt (NonNegative n) x =
  shrinkUInt x == (shrinkUInt . padUInt n $ x)

padSInt :: Int -> SInt -> SInt
padSInt n (SInt xs) = SInt (xs ++ replicate (n - length xs) (last xs))

shrinkSInt :: SInt -> SInt
shrinkSInt (SInt xs) = SInt . dropWhileEnd (== last xs) $ xs


-----
-- Basic numeric operations
-----

assertEqSInt :: SInt -> SInt -> SymEval ()
assertEqSInt x@(SInt xs) y@(SInt ys) = assertEqNGate px py
  where n = max (length xs) (length ys)
        (SInt px) = padSInt n x
        (SInt py) = padSInt n y

sint2UInt :: SInt -> SymEval UInt
sint2UInt (SInt xs) = do
  assertBoolGate False (last xs)
  return . UInt . init $ xs

uint2SInt :: UInt -> SInt
uint2SInt (UInt xs) = SInt (xs ++ [zero])

sUInt2SInt :: Bit -> UInt -> SymEval SInt
sUInt2SInt (Val False) u = return . uint2SInt $ u
sUInt2SInt (Val True)  u = neGate . uint2SInt $ u
sUInt2SInt sign u@(UInt xs) = do
  n <- neGate (uint2SInt u)
  let (SInt ys) = n
      px = xs ++ replicate (length ys - length xs) zero
  zs <- muxNGate sign px ys
  return . SInt $ zs

-- This test should still work when removing the optimizations when the sign is a value
prop_sUInt2SInt :: Integral n => Bool -> (NonNegative n) -> Bool
prop_sUInt2SInt b (NonNegative n) =
  if b
    then -n == (fromJust . sint2Int . eval . sUInt2SInt one  . int2UInt $ n)
    else  n == (fromJust . sint2Int . eval . sUInt2SInt zero . int2UInt $ n)

shiftUInt :: Int -> UInt -> UInt
shiftUInt n (UInt xs)
  | n < 0             = UInt $ (drop (-n) xs)
  | otherwise         = UInt $ replicate n zero ++ xs

shiftSInt :: Int -> SInt -> SInt
shiftSInt n (SInt xs)
  | -n >= (length xs) = SInt [last xs]
  | n < 0             = SInt $ drop (-n) xs
  | otherwise         = SInt $ replicate n zero ++ xs

truncateUInt :: Int -> UInt -> SymEval UInt
truncateUInt size (UInt xs)
  | size >= (length xs) = return (UInt xs)
  | otherwise           = do
      debugComment $ "truncate: " ++ show (size, xs)
      let (ys, zs) = splitAt size xs
      mapM_ (assertBoolGate False) zs
      debugComment $ "truncated: " ++ show ys
      return . UInt $ ys

truncateSInt :: Int -> SInt -> SymEval SInt
truncateSInt size (SInt xs)
  | size >= (length xs) = return (SInt xs)
  | otherwise           = do
      debugComment $ "truncate: " ++ show (size, xs)
      let (ys, zs) = splitAt size xs
      mapM_ (assertEqGate (last xs)) zs
      let res = ys ++ [last xs]
      debugComment $ "truncated: " ++ show res
      return . SInt $ res

-- rippleAdd with carry in, xs and ys must be equal length
rippleAddCarry :: [Bit] -> [Bit] -> Bit -> SymEval [Bit]
rippleAddCarry [] [] ci = return [ci]
rippleAddCarry (x:xs) (y:ys) ci = do
  (so, co) <- fullAdd x y ci
  ss <- rippleAddCarry xs ys co
  return (so : ss)

rippleAddUInts :: UInt -> UInt -> SymEval UInt
rippleAddUInts ux@(UInt xs) uy@(UInt ys) = do
  debugComment $ "Start RA: " ++ show (ux, uy)
  let n = max (length xs) (length ys)
      px = padUInt n ux
      py = padUInt n uy
  zs <- rippleAddCarry (getUInt px) (getUInt py) zero
  let res = UInt zs
  debugComment $ "End RA: " ++ show (ux, uy) ++ " -> " ++ show res
  return res

prop_rippleAddUInts :: Integral n => (NonNegative n) -> (NonNegative n) -> Bool
prop_rippleAddUInts (NonNegative x) (NonNegative y) =
  x + y == (fromJust . uint2Int . eval $ rippleAddUInts (int2UInt x) (int2UInt y))

rippleAddSInts :: SInt -> SInt -> SymEval SInt
rippleAddSInts sx@(SInt xs) sy@(SInt ys) = do
  debugComment $ "Start RA: " ++ show (sx, sy)
  let n = 1 + max (length xs) (length ys)
      px = padSInt n sx
      py = padSInt n sy
  zs <- rippleAddCarry (getSInt px) (getSInt py) zero
  let res = SInt (init zs)
  debugComment $ "End RA: " ++ show (sx, sy) ++ " -> " ++ show res
  return res

prop_rippleAddSInts :: Integral n => n -> n -> Bool
prop_rippleAddSInts x y =
  x + y == (fromJust . sint2Int . eval $ rippleAddSInts (int2SInt x) (int2SInt y))

addOneUInt :: UInt -> SymEval UInt
addOneUInt (UInt xs) = go one xs >>= return . UInt
  where go carry []     = return [carry]
        go carry (y:ys) = do
          (s, c) <- halfAdd carry y
          zs <- go c ys
          return (s:zs)

prop_addOneUInt :: Integral n => NonNegative n -> Bool
prop_addOneUInt (NonNegative n) =
  n + 1 == (fromJust . uint2Int . eval . addOneUInt . int2UInt $ n)

neGate :: SInt -> SymEval SInt
neGate (SInt xs) = do
  let as = xs ++ [last xs]
      bs = map neg as
  cs <- addOneUInt (UInt bs)
  return . SInt . init . getUInt $ cs

prop_neGate :: Integral n => n -> Bool
prop_neGate n = -n == (fromJust . sint2Int . eval . neGate . int2SInt $ n)

absGate :: SInt -> SymEval UInt
absGate s = do
  let sign = getSign s
  n <- neGate s
  debugComment $ "(s,n) " ++ show (s,n)
  zs <- muxNGate sign (getSInt s) (getSInt n)
  return . UInt $ zs
  
prop_absGate :: Integral n => n -> Bool
prop_absGate n = abs n == (fromJust . uint2Int . eval . absGate . int2SInt $ n)

rippleSubSInts :: SInt -> SInt -> SymEval SInt
rippleSubSInts sx@(SInt xs) sy@(SInt ys) = do
  debugComment $ "Start RS: " ++ show (sx, sy)
  let n = 1 + max (length xs) (length ys)
      px = padSInt n sx
      py = padSInt n sy
      ny = map neg (getSInt py)
  zs <- rippleAddCarry (getSInt px) ny one
  let res = SInt (init zs)
  debugComment $ "End RS: " ++ show (sx, sy) ++ " -> " ++ show res
  return res

prop_rippleSubSInts :: Integral n => n -> n -> Bool
prop_rippleSubSInts x y =
  x - y == (fromJust . sint2Int . eval $ rippleSubSInts (int2SInt x) (int2SInt y))

-- TODO this pads more than required
rippleSubUInts :: UInt -> UInt -> SymEval SInt
rippleSubUInts x y = rippleSubSInts (uint2SInt x) (uint2SInt y)

prop_rippleSubUInts :: Integral n => (NonNegative n) -> (NonNegative n) -> Bool
prop_rippleSubUInts (NonNegative x) (NonNegative y) =
  x - y == (fromJust . sint2Int . eval $ rippleSubUInts (int2UInt x) (int2UInt y))

longMult :: UInt -> UInt -> SymEval UInt
longMult ux@(UInt xs) uy@(UInt ys) = do
  debugComment $ "Start LM: " ++ show (ux, uy)
  zs <- mapM (\y -> mapM (andGate y) xs) ys
  let ws = zipWith shiftUInt [0..] . map UInt $ zs
  res <- sumUIntGate ws
  debugComment $ "End LM: " ++ show (ux, uy) ++ " -> " ++ show res
  return res

prop_longMult :: Integral n => (NonNegative n) -> (NonNegative n) -> Bool
prop_longMult (NonNegative x) (NonNegative y) =
  x * y == (fromJust . uint2Int . eval $ longMult (int2UInt x) (int2UInt y))

signedMult :: SInt -> SInt -> SymEval SInt
signedMult sx sy = do
  ux <- absGate sx
  uy <- absGate sy
  uz <- longMult ux uy
  sign <- xorGate (getSign sx) (getSign sy)
  sUInt2SInt sign uz

prop_signedMult :: Integral n => n -> n -> Bool
prop_signedMult x y =
  x * y == (fromJust . sint2Int . eval $ signedMult (int2SInt x) (int2SInt y))

sqrUInt :: UInt -> SymEval UInt
sqrUInt u@(UInt xs) = do
  debugComment $ "Start sqr: " ++ show u
  ys <- go xs
  let zs = zipWith shiftUInt [0,2..] . map UInt $ ys
  res <- sumUIntGate zs
  debugComment $ "End sqr: " ++ show (xs, ys) ++ " -> " ++ show res
  return res
  where go [] = return []
        go (a:as) = do
          bs <- mapM (andGate a) as
          cs <- go as
          return $ (a:zero:bs) : cs


prop_sqrUInt :: Integral n => NonNegative n -> Bool
prop_sqrUInt (NonNegative x) =
  x^2 == (fromJust . uint2Int . eval . sqrUInt . int2UInt $ x)

power :: Maybe Int -> UInt -> UInt -> SymEval UInt
power size x uy@(UInt ys) = do
  debugComment $ "power: " ++ show (size, x, uy)
  xs <- squares (length ys - 1) x
  zs <- zipWithM select ys (x:xs)
  res <- productGate size zs
  debugComment $ "power result: " ++ show (size, x, ys) ++ " -> " ++ show res
  return res
  where squares 0 a = return []
        squares n a = do
          a2 <- sqrUInt a
          as <- squares (n-1) a2
          return (a2:as)
        select s (UInt bits) = muxNGate s one' bits >>= return . UInt
          where (UInt one') = padUInt (length bits) uintOne

prop_power :: Integral n => (NonNegative n) -> (NonNegative n) -> Bool
prop_power (NonNegative x) (NonNegative y) =
  x ^ y == (fromJust . uint2Int . eval $ power Nothing (int2UInt x) (int2UInt y))

powers :: UInt -> Int -> SymEval [UInt]
powers _ 0 = return [uintOne]
powers x n = go [x, uintOne] 2
  where go acc i | i > n  = return (reverse acc)
                 | even i = do s <- sqrUInt (acc !! (div (length acc) 2 - 1))
                               go (s:acc) (i + 1)
                 | odd i  = do s <- longMult (head acc) x
                               go (s:acc) (i + 1)

prop_powers :: Integral n => NonNegative n -> NonNegative Int -> Bool
prop_powers (NonNegative x) (NonNegative y) =
  map (x^) [0..y] == (map (fromJust . uint2Int) . eval $ powers (int2UInt x) y)


-----
-- Numeric gates over multiple numbers
-----

sumUIntGate :: [UInt] -> SymEval UInt
sumUIntGate uints = do
  let bss = map getUInt uints
      tbs = transpose bss
  debugComment $ "sum: " ++ show uints
  ys <- addCols tbs
  let res = UInt ys
  debugComment $ "sum result: " ++ show uints ++ " -> " ++ show res
  return res
  where
    addCols :: [[Bit]] -> SymEval [Bit]
    addCols []     = return []
    addCols [[]]   = return []
    addCols [c]    = do
      (s, carries) <- addCol c
      ds <- addCols [carries]
      return (s:ds)
    addCols (c:c':cs) = do
      (s, carries) <- addCol c
      ds <- addCols ((c' ++ carries):cs)
      return (s:ds)
    
    addCol :: [Bit] -> SymEval (Bit, [Bit])
    addCol []  = return (zero, [])
    addCol [x] = return (x, [])
    addCol [x,y] = do
      (s, c) <- halfAdd x y
      return (s, [c])
    addCol (x:y:z:xs) = do
      (s, c) <- fullAdd x y z
      (s', cs) <- addCol (s:xs)
      return $ (s', c:cs)

prop_sumUIntGate :: Integral n => [NonNegative n] -> Bool
prop_sumUIntGate xs = let ns = map getNonNegative xs
  in sum ns == (fromJust . uint2Int . eval . sumUIntGate . map int2UInt $ ns)

productGate :: (Maybe Int) -> [UInt] -> SymEval UInt
productGate (Just size) ns = do
  debugComment $ "product: " ++ show (size, ns)
  res <- treeFoldM truncMult uintOne ns
  debugComment $ "product res: " ++ show (size, ns) ++ " -> " ++ show res
  return res
  where truncMult x y = longMult x y >>= truncateUInt size
productGate Nothing ns = do
  debugComment $ "product: " ++ show ns
  res <- treeFoldM longMult uintOne ns
  debugComment $ "product res: " ++ show ns ++ " -> " ++ show res
  return res

prop_productGate :: Integral n => [NonNegative n] -> Bool
prop_productGate xs = let ns = map getNonNegative xs
  in product ns == (fromJust . uint2Int . eval . productGate Nothing . map int2UInt $ ns)

prop_truncProductGate :: Integral n => [NonNegative n] -> Bool
prop_truncProductGate xs =
  p == (fromJust . uint2Int . eval . productGate (Just sp) $ pns)
  where ns = map getNonNegative xs
        p = product ns
        sp = (+1) . ceiling . logBase 2 . fromIntegral $ p
        uints = map int2UInt ns
        sns = 1 + (maximum . map (length . getUInt) $ uints)
        pns = map (padUInt sns) uints

-----
-- Specifics for smoothness testing
-----

-- prod p^e 
primeProduct :: (Integral n, Show n) => Maybe Int -> [(n, UInt)] -> SymEval SInt
primeProduct size x@((-1, UInt [sign]):pes) = do
  debugComment $ "primePowers: " ++ show (size, x)
  pps <- mapM (\(p, e) -> power size (int2UInt p) e) pes
  prod <- productGate size pps
  res <- sUInt2SInt sign prod
  debugComment $ "primePowers result: " ++ show (size, x) ++ " -> " ++ show res
  return res

rationalSide :: SInt -> UInt -> UInt -> SymEval SInt
rationalSide a b m = do
  debugComment $ "rationalSide: " ++ show (a,b,m)
  assertOrNGate (getUInt b)
  bm <- longMult b m
  res <- rippleAddSInts a (uint2SInt bm)
  debugComment $ "rationalSide result: " ++ show (a,b,m) ++ " -> " ++ show res
  return res

-- g(a, b)
algebraicSide :: SInt -> UInt -> [UInt] -> Int -> SymEval SInt
algebraicSide a b cs lgBound = do
  debugComment $ "algebraicSide: " ++ show (a,b,cs)
  let d = length cs - 1
  aa <- absGate a
  debugComment $ "abs a: " ++ show aa
  pas <- powers aa d
  pbs <- powers b d 
  pabs <- zipWithM truncMult pas (reverse pbs)
  debugComment $ "pabs: " ++ show pabs
  terms <- zipWithM truncMult cs pabs
  debugComment $ "terms: " ++ show terms
  let (evens, odds) = evenOdd terms
  -- sum the terms, postponing negation
  sEvens <- sumUIntGate evens
  debugComment $ "sEvens: " ++ show sEvens
  sOdds <- sumUIntGate odds
  debugComment $ "sOdds: " ++ show sOdds
  res <- if even d then do so <- sUInt2SInt (neg . getSign $ a) sOdds
                           debugComment $ "d even: " ++ show so
                           rippleAddSInts so (uint2SInt sEvens)
                   else do so <- sUInt2SInt (getSign a) sOdds
                           rippleSubSInts so (uint2SInt sEvens)
  debugComment $ "algebraicSide: " ++ show (a,b,cs) ++ " -> " ++ show res
  return res
  where truncMult x y = longMult x y >>= truncateUInt lgBound

-- F(a,b)
nfsSide :: SInt -> UInt -> UInt -> [UInt] -> Int -> SymEval SInt
nfsSide a b m cs lgBound = do
  debugComment $ "nfsSide: " ++ show (a,b,m,cs)
  rat <- rationalSide a b m
  alg <- algebraicSide a b cs lgBound
  res <- signedMult rat alg >>= truncateSInt lgBound
  debugComment $ "nfsSide result: " ++ show (a,b,m,cs) ++ " -> " ++ show res
  return res

smoothCircuit :: (Integral n, Show n) => SInt -> UInt -> UInt -> [UInt] -> [(n, UInt)] -> Int -> SymEval ()
smoothCircuit a b m cs pes lgBound = do
  debugComment $ "smoothCircuit: " ++ show (a, b, m, cs, pes)
  lhs <- nfsSide a b m cs lgBound
  rhs <- primeProduct (Just lgBound) pes
  debugComment $ "smoothCircuit result: " ++ show lhs ++ " == " ++ show rhs
  assertEqSInt lhs rhs


-- L_N(1/3, c) = exp[ (c + o(1)) (log N)^(1/3) (log log N)^(2/3) ]
-- assuming o(1) = 0
bigl :: Floating f => f -> f -> f
bigl c n = exp (c * (ln ** (1/3)) * (lln ** (2/3)))
  where lln = log ln
        ln = log n

primeList :: (Integral n, Show n, Floating f, RealFrac f, Show f) => n -> f -> f -> f -> [n] -> f -> Maybe Int -> SymEval [(n, UInt)]
primeList m d y u cs bound eMax = do
  sign <- nextVar
  let y' = floor y
      ps = takeWhile (<= y') primes
      m' = fromIntegral m
      getExp p = do
        let p' = fromIntegral p
            e_bound = logBase p' bound
            lge_bound = floor . logBase 2 $ e_bound
            e_size = maybe lge_bound (min lge_bound) eMax
        e <- replicateM e_size nextVar
        return (p, UInt e)
  rest <- mapM getExp ps
  return $ (-1, UInt [sign]) : rest

-- steps 1-3 of the algorithm
smoothInput :: (Integral n, Show n, Floating f, RealFrac f, Show f) => n -> f -> f -> f -> Maybe Int -> SymEval (SInt, UInt, UInt, [UInt], [(n, UInt)], Int)
smoothInput n d y u eMax = do
  let lgu = ceiling $ logBase 2 u
  a <- replicateM (lgu+1) nextVar 
  addComment $ "a := " ++ show a
  b <- replicateM lgu nextVar
  addComment $ "b := " ++ show b
  let n' = fromIntegral n
      m = floor (n' ** (1 / d))
      mbits = int2UInt m
  addComment $ "m = " ++ show m
  let cs = digits m n
      cbits = map int2UInt cs
  addComment $ "cs = " ++ show cs
  let bound = u**(fromIntegral (length cs) + 1) * (fromIntegral m + 1) * (fromIntegral (sum cs))
      lgBound = ceiling $ logBase 2 bound
  pes <- primeList m d y u cs bound eMax
  mapM_ (\(p, e) -> addComment $ "pp := " ++ show p ++ "^" ++ show (getUInt e)) pes
  return (SInt a, UInt b, mbits, cbits, pes, lgBound)

-- Compute parameters according to N as given in [BBM17]
smooth :: (Integral n, Show n) => n -> Maybe Int -> SymEval ()
smooth n eMax = do
  addComment $ "smooth (n, eMax): " ++ show (n, eMax)
  let beta = (8/9)**(1/3) :: Double
      delta = 3**(1/3) :: Double
      epsilon = beta
  addComment $ "parameters (beta, delta, epsilon): " ++ show (beta, delta, epsilon)
  let n' = fromIntegral n
      ln = log n'
      lln = log ln
      d = delta * (ln**(1/3)) * (lln**(-1/3))
      y = bigl beta n'
      u = bigl epsilon n'
  addComment $ "parameters (d, y, u): " ++ show (d, y, u)
  (a, b, m, cs, pes, lgBound) <- smoothInput n d y u eMax
  smoothCircuit a b m cs pes lgBound

-----
-- Main
-----

main = do
  args <- getArgs
  pname <- getProgName
  case args of
    [sn] -> let n = read sn
             in putStr . seToDimacs $ smooth n Nothing
    [sn, seMax] -> let n = read sn
                       eMax = read seMax    
                    in putStr . seToDimacs $ smooth n (Just eMax)
    _  -> hPutStrLn stderr $ "Usage " ++ pname ++ " N [eMax]"


-----
-- Testing
-----

return []
runTests = $quickCheckAll


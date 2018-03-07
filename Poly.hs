import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe

-- A loose interpretation of the polynomial exercises in Section 2.5 of SICP:
-- polymorphism in polynomial coefficients

-- A semiring: something with addition and multiplication (but not necessarily
-- negation)
class (Eq a, Show a) => Semiring a where
  (|+|):: a -> a -> a
  (|*|):: a -> a -> a
  zero:: a          -- identity for +
  one:: a           -- identity for *

-- Negation
class Negatable a where
  neg:: a -> a

-- A ring: a semiring with negation
class (Semiring a, Negatable a) => Ring a where
  (|-|):: a -> a -> a
  x |-| y = x |+| neg y

-- Integer addition and multiplication form a ring

-- If we were brave enough to press the UndeciableInstances button, we could
-- also say something like:
-- 
--    instance (Num a) => Semiring a where ...
--    instance (Num a) => Negatable a where ...

instance Semiring Integer where
  m |+| n = m + n
  m |*| n = m * n
  zero = 0
  one = 1

instance Negatable Integer where
  neg n = negate n

instance Ring Integer

data Nat = Z | S Nat deriving Eq

nat:: Integer -> Nat
nat 0 = Z
nat n = if n < 0 then error "No negative nats!" else S (nat (n - 1))

instance Show Nat where
  show n = show (countSs n)
           where
             countSs:: Nat -> Integer
             countSs Z = 0
             countSs (S m) = 1 + (countSs m)

instance Semiring Nat where
  Z |+| n = n
  S m |+| n = m |+| S n
  Z |*| n = Z
  S m |*| n = n |+| (m |*| n)
  zero = Z
  one = S Z

instance Ord Nat where
  Z <= _ = True
  S _ <= Z = False
  S m <= S n = m <= n
  
-- Univariate polynomial with coefficients of type c.
-- Represented as a map from the exponenent n (of type Nat) of each term to the
-- coefficient for that term (using a map accommodates sparse polynomials).
-- Here we would like the type of Poly to depend on a *value* for the variable
-- that is the indeterminate of the polynoomial, but we don't have dependent
-- types. Instead we kluge the name of the variable as a Maybe String, where
-- Nothing means that the variable name is unassigned (as in the polynomial
-- zero, which has no terms).
data Poly c = Poly (Maybe String) (M.Map Nat c) deriving Eq

inVar:: Poly c -> Maybe String -> Poly c
inVar (Poly v m) v' = Poly v' m

-- Multiply a polynomial by a term in the same variable (where the term is
-- represented as an exponent and coefficient)
mulPolyByExpCoeff:: Semiring c => Poly c -> (Nat,c) -> Poly c
mulPolyByExpCoeff (Poly v m) (n,c) =
  Poly v $ M.fromList $ map (expCoeffProd (n,c)) (M.toList m)
  where expCoeffProd (n1,c1) (n2,c2) = (n1 |+| n2,c1 |*| c2)
    
compatibleVariable:: Maybe String -> Maybe String -> Maybe String
compatibleVariable Nothing Nothing = Nothing
compatibleVariable Nothing (Just s) = (Just s)
compatibleVariable (Just s) Nothing = (Just s)
compatibleVariable (Just s1) (Just s2) =
  if s1 == s2 then Just s1
  else error ("Incompatible variables " ++ s1 ++ " and " ++ s2)

instance (Show c) => Show (Poly c) where
  show (Poly v m) =
    if M.null m then
      "0"
    else
      concat $ intersperse " + " $ map termString $ reverse $ M.toList m
      where vName = fromMaybe "<>" v
            termString (Z,c) = show c
            termString (S Z,c) = show c ++ "*" ++ vName
            termString (n,c) = show c ++ "*" ++ vName ++ "^" ++ show n

instance (Semiring c) => Semiring (Poly c) where
  Poly v1 m1 |+| Poly v2 m2 =
    let v = compatibleVariable v1 v2
        allKeys = nub (M.keys m1 ++ M.keys m2)
    in  Poly v (M.fromList $ map (\n -> (n,coeffSum n)) $ allKeys)
    where
      coeffSum n = M.findWithDefault zero n m1 |+| M.findWithDefault zero n m2
  Poly v1 m1 |*| Poly v2 m2 =
    let v = compatibleVariable v1 v2
        prods = map (mulPolyByExpCoeff (Poly v1 m1)) (M.toList m2)
    in  foldr (|+|) zero prods `inVar` v
  zero = Poly Nothing M.empty
  one = Poly Nothing (M.fromList [(nat 0,one)])

instance (Negatable c) => Negatable (Poly c) where
  neg (Poly v m) = Poly v (M.map neg m)

instance (Semiring c, Negatable c) => Ring (Poly c)

testPolys:: [Poly Integer]
testPolys = [
  zero,
  one,
  Poly (Just "x") (M.fromList [(nat 3,1),(nat 0,1)]),
  Poly (Just "x") (M.fromList [(nat 4,4),(nat 2,2),(nat 1,1)])]

polyTest:: (Show c, Ring c) => Poly c -> Poly c -> String
polyTest p0 p1 =
  show p0 ++ "  |+|  " ++ show p1 ++ "  =  " ++ show (p0 |+| p1) ++ "\n" ++
  show p0 ++ "  |-|  " ++ show p1 ++ "  =  " ++ show (p0 |-| p1) ++ "\n" ++
  show p0 ++ "  |*|  " ++ show p1 ++ "  =  " ++ show (p0 |*| p1) ++ "\n"
  
allTests:: [String]
allTests =  do
  a <- testPolys
  b <- testPolys
  return (polyTest a b)

main = putStrLn (concat allTests)

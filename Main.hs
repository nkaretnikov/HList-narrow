-- From HList-0.2.3.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Proxy

main :: IO ()
main = undefined

------------------------------------------------------------

-- | List constructors that also LUB together

-- A type-level code for the empty list
data NilLub
-- The empty list constructor
nilLub = undefined :: NilLub

-- Cons as a type-level function
class ConsLub h t l | h t -> l where
  consLub :: h -> t -> l

-- No coercion needed for a singleton list
instance ConsLub e NilLub [e] where
  consLub h _ = [h]

------------------------------------------------------------

-- Narrow head and tail to their LUB type
instance LubNarrow e0 e1 e2 => ConsLub e0 [e1] [e2] where
  consLub h t = fst (head z) : map snd (tail z)
    where
      z = map (lubNarrow h) (undefined:t)

-- The important operation is lubNarrow:
class LubNarrow a b c | a b -> c where
  lubNarrow :: a -> b -> (c,c)

------------------------------------------------------------

data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)

-- * Booleans
data HTrue

hTrue :: HTrue
hTrue = undefined

data HFalse
hFalse :: HFalse
hFalse = undefined

instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"

------------------------------------------------------------

-- Projection

-- | Field of label l with value type v
newtype LVPair l v = LVPair { valueLVPair :: v } deriving Eq

-- | Label accessor
labelLVPair :: LVPair l v -> l
labelLVPair = undefined

newLVPair :: l -> v -> LVPair l v
newLVPair _ = LVPair

newtype Record r = Record r deriving Eq

------------------------------------------------------------

-- * Error messages

-- | A class without instances for explicit failure
class Fail x

------------------------------------------------------------

class HBool x
instance HBool HTrue
instance HBool HFalse

------------------------------------------------------------

-- * Equality for types

class HBool b => HEq x y b | x y -> b

------------------------------------------------------------

-- | A predicate for type equality

class HBool b => TypeEq x y b | x y -> b

-- Rely on lazy show for type-level Booleans
typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined

-- A more disciplined version: based on proxies
proxyEq :: TypeEq t t' b => Proxy t -> Proxy t' -> b
proxyEq _ _ = undefined

------------------------------------------------------------

-- * Naturals

data HZero
data HSucc n

hZero :: HZero
hZero = undefined

hSucc :: HNat n => n -> HSucc n
hSucc _ = undefined

hPred :: HNat n => HSucc n -> n
hPred _ = undefined

------------------------------------------------------------

class HNat n
instance HNat HZero
instance HNat n => HNat (HSucc n)

instance Show HZero where
  show _ = "HZero"

instance Show (HSucc HZero) where
  show _ = "HSucc HZero"

instance (HNat n, Show (HSucc n)) => Show (HSucc (HSucc n)) where 
  show n = "HSucc (" ++ show (hPred n) ++ ")"

------------------------------------------------------------

class HNat n => HNat2Integral n where
  hNat2Integral :: Integral i => n -> i

instance HNat2Integral HZero where
  hNat2Integral _ = 0

instance HNat2Integral n => HNat2Integral (HSucc n) where
  hNat2Integral n = hNat2Integral (hPred n) + 1

------------------------------------------------------------

-- Equality instances for naturals

instance HEq HZero HZero HTrue
instance HNat n => HEq HZero (HSucc n) HFalse
instance HNat n => HEq (HSucc n) HZero HFalse
instance (HNat n, HNat n', HEq  n n' b )
      =>  HEq (HSucc n) (HSucc n') b

hEq :: HEq x y b => x -> y -> b
hEq =  undefined

------------------------------------------------------------

-- ** Disjunction

class (HBool t, HBool t', HBool t'') => HOr t t' t'' | t t' -> t''
 where
  hOr :: t -> t' -> t''

instance HOr HFalse HFalse HFalse
 where
  hOr _ _ = hFalse

instance HOr HTrue HFalse HTrue
 where
  hOr _ _ = hTrue

instance HOr HFalse HTrue HTrue
 where
  hOr _ _ = hTrue

instance HOr HTrue HTrue HTrue
 where
  hOr _ _ = hTrue

------------------------------------------------------------

-- ** Membership test based on type equality

class HBool b => HTMember e l b | e l -> b
instance HTMember e HNil HFalse
instance (TypeEq e e' b, HTMember e l b', HOr b b' b'')
      =>  HTMember e (HCons e' l) b''

hTMember :: HTMember e l b => e -> l -> b
hTMember _ _ = undefined


------------------------------------------------------------

-- | Zip and unzip
class HZip x y l | x y -> l, l -> x y
 where
  hZip   :: x -> y -> l
  hUnzip :: l -> (x,y)

instance HZip HNil HNil HNil
 where
  hZip HNil HNil = HNil
  hUnzip HNil = (HNil,HNil)

instance HZip tx ty l
      => HZip (HCons hx tx) (HCons hy ty) (HCons (hx,hy) l)
 where
  hZip (HCons hx tx) (HCons hy ty) = HCons (hx,hy) (hZip tx ty)
  hUnzip (HCons (hx,hy) l) = (HCons hx tx, HCons hy ty)
   where
    (tx,ty) = hUnzip l

------------------------------------------------------------

-- | Intersection based on HTMember
class HTIntersect l1 l2 l3 | l1 l2 -> l3
 where
  -- | Like 'Data.List.intersect'
  hTIntersect :: l1 -> l2 -> l3

instance HTIntersect HNil l HNil
 where
  hTIntersect _ _ = HNil

instance ( HTMember h l1 b
         , HTIntersectBool b h t l1 l2
         )
         => HTIntersect (HCons h t) l1 l2
 where
  hTIntersect (HCons h t) l1 = hTIntersectBool b h t l1
   where
    b = hTMember h l1


class HBool b => HTIntersectBool b h t l1 l2 | b h t l1 -> l2
 where
 hTIntersectBool :: b -> h -> t -> l1 -> l2

instance HTIntersect t l1 l2
      => HTIntersectBool HTrue h t l1 (HCons h l2)
 where
  hTIntersectBool _ h t l1 = HCons h (hTIntersect t l1)

instance HTIntersect t l1 l2
      => HTIntersectBool HFalse h t l1 l2
 where
  hTIntersectBool _ _ t l1 = hTIntersect t l1

------------------------------------------------------------

-- | Propery of a proper label set for a record: no duplication of labels

class HRLabelSet ps
instance HRLabelSet HNil
instance HRLabelSet (HCons x HNil)
instance ( HEq l1 l2 leq
         , HRLabelSet' l1 v1 l2 v2 leq r
         ) => HRLabelSet (HCons (LVPair l1 v1) (HCons (LVPair l2 v2) r))


class HRLabelSet' l1 v1 l2 v2 leq r
instance ( HRLabelSet (HCons (LVPair l2 v2) r)
         , HRLabelSet (HCons (LVPair l1 v1) r)
         ) => HRLabelSet' l1 v1 l2 v2 HFalse r
instance ( Fail (DuplicatedLabel l1) ) => HRLabelSet' l1 v1 l2 v2 HTrue r

{-
instance (HZip ls vs ps, HLabelSet ls) => HRLabelSet ps
-}

-- * Membership test

class HBool b => HMember e l b | e l -> b
instance HMember e HNil HFalse
instance (HEq e e' b, HMember e l b', HOr b b' b'')
      =>  HMember e (HCons e' l) b''

hMember :: HMember e l b => e -> l -> b
hMember _ _ = undefined

-- ** Another type-level membership test
--
-- Check to see if an element e occurs in a list l
-- If not, return HNothing
-- If the element does occur, return HJust l'
-- where l' is a type-level list without e

class HMemberM e l r | e l -> r
instance HMemberM e HNil HNothing
instance (HEq e e' b, HMemberM' b e (HCons e' l) res)
      =>  HMemberM e (HCons e' l) res
class HMemberM' b e l r | b e l -> r
instance HMemberM' HTrue e (HCons e l) (HJust l)
instance (HMemberM e l r, HMemberM' r e (HCons e' l) res)
    => HMemberM' HFalse e (HCons e' l) res
instance HMemberM' HNothing e l HNothing
instance HMemberM' (HJust l') e (HCons e' l) (HJust (HCons e' l'))



class HLabelSet ls
instance HLabelSet HNil
instance (HMember x ls xmem, HLabelSet' x ls xmem) => HLabelSet (HCons x ls)

class HLabelSet' x ls xmem
instance HLabelSet ls => HLabelSet' x ls HFalse

data DuplicatedLabel l = DuplicatedLabel l
instance Fail (DuplicatedLabel x) => HLabelSet' x ls HTrue


-- | Build a record
mkRecord :: HRLabelSet r => r -> Record r
mkRecord = Record

-- | Build an empty record
emptyRecord :: Record HNil
emptyRecord = mkRecord HNil

-- | @hProjectByLabels ls r@ returns @r@ with only the labels in @ls@ remaining
hProjectByLabels :: (HRLabelSet a, H2ProjectByLabels ls t a b) => ls -> Record t -> Record a
hProjectByLabels ls (Record r) = mkRecord (fst $ h2projectByLabels ls r)

-- | See 'H2ProjectByLabels'
hProjectByLabels2 :: (H2ProjectByLabels ls t t1 t2, HRLabelSet t1, HRLabelSet t2) =>ls -> Record t -> (Record t1, Record t2)
hProjectByLabels2 ls (Record r) = (mkRecord rin, mkRecord rout)
   where (rin,rout) = h2projectByLabels ls r

-- | /Invariant/:
--
--  > r === rin `disjoint-union` rout
--  > labels rin === ls
--  >     where (rin,rout) = hProjectByLabels ls r
class H2ProjectByLabels ls r rin rout | ls r -> rin rout where
    h2projectByLabels :: ls -> r -> (rin,rout)

instance H2ProjectByLabels HNil r HNil r where
    h2projectByLabels _ r = (HNil,r)

instance H2ProjectByLabels (HCons l ls) HNil HNil HNil where
    h2projectByLabels _ _ = (HNil,HNil)

{-
instance (HMemberM l' (HCons l ls) b,
          H2ProjectByLabels' b (HCons l ls) (HCons (LVPair l' v') r') rin rout)
    => H2ProjectByLabels (HCons l ls) (HCons (LVPair l' v') r') rin rout where
    -- h2projectByLabels = h2projectByLabels' (undefined::b)
    -- The latter is solely for the Hugs benefit
    h2projectByLabels ls r@(HCons _ _) =h2projectByLabels' (undefined::b) ls r
      -- where b = hMember (labelLVPair f') ls
-}

class H2ProjectByLabels' b ls r rin rout | b ls r -> rin rout where
    h2projectByLabels' :: b -> ls -> r -> (rin,rout)

-- * Maybies

data HNothing  = HNothing  deriving Show
data HJust x   = HJust x   deriving Show


{-
instance H2ProjectByLabels ls' r' rin rout =>
    H2ProjectByLabels' (HJust ls') ls (HCons f' r') (HCons f' rin) rout where
    h2projectByLabels' _ _ (HCons x r) = (HCons x rin, rout)
        where (rin,rout) = h2projectByLabels (undefined::ls') r
-}

instance H2ProjectByLabels ls r' rin rout =>
    H2ProjectByLabels' HNothing ls (HCons f' r') rin (HCons f' rout) where
    h2projectByLabels' _ ls (HCons x r) = (rin, HCons x rout)
        where (rin,rout) = h2projectByLabels ls r

instance
  ( HZip la va a
  , HZip lb vb b
  , HTIntersect la lb lc
  , H2ProjectByLabels lc a c aout
  , H2ProjectByLabels lc b c bout
  , HRLabelSet c
  ) => LubNarrow (Record a) (Record b) (Record c) where
  lubNarrow ra@(Record a) rb@(Record b) =
    ( hProjectByLabels lc ra
    , hProjectByLabels lc rb
    )
    where
      lc = hTIntersect la lb
      (la,_) = hUnzip a
      (lb,_) = hUnzip b
-- That is, given two records ra and rb, we compute the intersection lc of their labels
-- la and lb such that we can subsequently project both records to this shared label
-- set.

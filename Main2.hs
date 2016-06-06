-- | A type-level code for the empty list.
data NilLub

-- | Cons as a type-level function.
class ConsLub h t l | h t -> l where
  consLub :: h -> t -> l
  
-- | Narrow to LUB type?
class LubNarrow a b c | a b -> c where
  lubNarrow :: a -> b -> (c,c)

-- * HList
data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)

-- * Booleans
data HTrue
data HFalse


-- | Field of label l with value type v.
newtype LVPair l v = LVPair { valueLVPair :: v } deriving Eq

newtype Record r = Record r deriving Eq

-- | A class without instances for explicit failure.
class Fail x

-- * Equality for types
class HBool x
instance HBool HTrue
instance HBool HFalse
class HBool b => HEq x y b | x y -> b
-- Equality instances for naturals
instance HEq HZero HZero HTrue
instance HNat n => HEq HZero (HSucc n) HFalse
instance HNat n => HEq (HSucc n) HZero HFalse
instance (HNat n, HNat n', HEq  n n' b )
      =>  HEq (HSucc n) (HSucc n') b

-- | A predicate for type equality
class HBool b => TypeEq x y b | x y -> b

-- * Naturals
data HZero
data HSucc n

class HNat n
instance HNat HZero
instance HNat n => HNat (HSucc n)

-- * Disjunction
class (HBool t, HBool t', HBool t'')
  -- t'' is uniquely determined from t and t'.
  => HOr t t' t'' | t t' -> t''
  where
    hOr :: t -> t' -> t''

instance HOr HFalse HFalse HFalse where hOr _ _ = hFalse
instance HOr HTrue HFalse HTrue   where hOr _ _ = hTrue
instance HOr HFalse HTrue HTrue   where hOr _ _ = hTrue
instance HOr HTrue HTrue HTrue    where hOr _ _ = hTrue

-- * Membership test based on type equality
class HBool b => HTMember e l b | e l -> b
instance
     HTMember e HNil         HFalse
instance (TypeEq e e' b, HTMember e l b', HOr b b' b'')
  => HTMember e (HCons e' l) b''

-- * Zip and unzip
class HZip x y l | x y -> l, l -> x y
 where
  hZip   :: x -> y -> l
  hUnzip :: l -> (x,y)

instance HZip HNil HNil HNil
  where
    hZip HNil HNil = HNil

    hUnzip HNil = (HNil,HNil)

instance HZip tx ty l
  => HZip (HCons hx      tx)
          (HCons hy      ty)
          (HCons (hx,hy) l)
 where
  hZip (HCons hx tx) (HCons hy ty) = HCons (hx,hy) (hZip tx ty)

  hUnzip (HCons (hx,hy) l) = (HCons hx tx, HCons hy ty)
   where
    (tx,ty) = hUnzip l

-- * Intersection based on HTMember
class HTIntersect l1 l2 l3 | l1 l2 -> l3 where
  -- | Like 'Data.List.intersect'
  hTIntersect :: l1 -> l2 -> l3

instance
  HTIntersect HNil l HNil
  where
    hTIntersect _ _ = HNil

instance
  ( HTMember h l1 b
  , HTIntersectBool b h t l1 l2
  )
  => HTIntersect (HCons h t) l1 l2
  where
    hTIntersect (HCons h t) l1 = hTIntersectBool b h t l1
      where
        b = hTMember h l1

class
  HBool b
  => HTIntersectBool b h t l1 l2 | b h t l1 -> l2
  where
    hTIntersectBool :: b -> h -> t -> l1 -> l2

instance
  HTIntersect t l1 l2
  => HTIntersectBool HTrue h t l1 (HCons h l2)
 where
   hTIntersectBool _ h t l1 = HCons h (hTIntersect t l1)

instance
  HTIntersect t l1 l2
  => HTIntersectBool HFalse h t l1 l2
  where
    hTIntersectBool _ _ t l1 = hTIntersect t l1

-- * Propery of a proper label set for a record: no duplication of labels
class HRLabelSet ps
instance HRLabelSet HNil
instance HRLabelSet (HCons x HNil)
instance
  ( HEq l1 l2 leq
  , HRLabelSet' l1 v1 l2 v2 leq r
  )
  => HRLabelSet (HCons (LVPair l1 v1) (HCons (LVPair l2 v2) r))

class HRLabelSet' l1 v1 l2 v2 leq r
instance
  ( HRLabelSet (HCons (LVPair l2 v2) r)
  , HRLabelSet (HCons (LVPair l1 v1) r)
  )
  => HRLabelSet' l1 v1 l2 v2 HFalse r
instance
  ( Fail (DuplicatedLabel l1)
  )
  => HRLabelSet' l1 v1 l2 v2 HTrue r

-- * Membership test
class HBool b => HMember e l b | e l -> b
instance HMember e HNil HFalse
instance (HEq e e' b, HMember e l b', HOr b b' b'')
      =>  HMember e (HCons e' l) b''

-- ** Another type-level membership test
--
-- Check to see if an element e occurs in a list l
-- If not, return HNothing
-- If the element does occur, return HJust l'
-- where l' is a type-level list without e
class HMemberM e l r | e l -> r
instance
  HMemberM e HNil HNothing
instance
  (HEq e e' b, HMemberM' b e (HCons e' l) res)
   =>  HMemberM e (HCons e' l) res

class HMemberM' b e l r | b e l -> r
instance
     HMemberM' HTrue e (HCons e l) (HJust l)
instance (HMemberM e l r, HMemberM' r e (HCons e' l) res)
  => HMemberM' HFalse e (HCons e' l) res
instance HMemberM'
  HNothing   e l            HNothing
instance HMemberM'
  (HJust l') e (HCons e' l) (HJust (HCons e' l'))

class HLabelSet ls
instance HLabelSet HNil
instance (HMember x ls xmem, HLabelSet' x ls xmem) => HLabelSet (HCons x ls)

class HLabelSet' x ls xmem
instance HLabelSet ls => HLabelSet' x ls HFalse

data DuplicatedLabel l = DuplicatedLabel l
instance Fail (DuplicatedLabel x) => HLabelSet' x ls HTrue


-- XXX: incomplete

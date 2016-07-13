{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
import Data.Word

-- Chapter exercises

-- 1. Bool has kind *, hence no Functor instance
-- 2. Kind * -> *, can have Functor
-- 3. Same
-- 4.
newtype Mu f = InF { outF :: f (Mu f) }
-- Mu :: (* -> *) -> *
-- Yes?

-- 5.
data D = D (Array Word Word) Int Int
-- D :: *
-- No!

-- Rearrange

-- 1.
data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum e) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2.
data Company a b c = DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

-- 3.
data More a b = L b a b | R a b a deriving (Eq, Show)
instance Functor (More x) where
  fmap f (R a b a') = R a (f b) a'
  fmap f (L b a b') = L (f b) a (f b')
                      
-- Write Functor instances
-- 1.
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant e) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)
-- 2.
data K a b = K a
instance Functor (K a) where
    fmap f (K b) = K b
-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))
-- 4.
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
-- 5.
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) =
      IgnoringSomething fa (fmap f gb)
-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
-- 9.
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)
-- 10.
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl gl' gl'') =
          MoreGoats (fmap f gl) (fmap f gl') (fmap f gl'')
-- 11.
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa) = Read (f . sa)

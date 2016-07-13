
import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Booly a = False'
             | True'
               deriving (Eq, Show)

instance Monoid (Booly a) where
  mempty = False'
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'

data Optional a = Nada
                | Only a
                  deriving (Eq, Show)

-- sample (genOnly :: Gen (Optional String))
genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  x <- arbitrary
  return $ Only x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, genOnly)
              , (1, return Nada) ]


instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only a') = Only (a <> a')
  mappend Nada Nada = Nada

---

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat [e,
                                           "! he said ",
                                           adv,
                                           " as he jumped into his car ",
                                           noun,
                                           " and drove off with his ",
                                           adj,
                                           " wife."]

----

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

type S = String
type B = Bool

data Bull = Fools
          | Twoo
            deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: BullMappend)
--   quickCheck (monoidLeftIdentity :: Bull -> Bool)
--   quickCheck (monoidRightIdentity :: Bull -> Bool)

----

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = (First' { getFirst' = Nada })
  mappend (First' { getFirst' = Nada })
              (First' { getFirst' = Nada }) =
              (First' { getFirst' = Nada })
  mappend (First' { getFirst' = Nada })
              (First' { getFirst' = Only a }) =
              (First' { getFirst' = Only a})
  mappend (First' { getFirst' = Only a })
              (First' { getFirst' = Nada }) =
              (First' { getFirst' = Only a })
  mappend (First' { getFirst' = Only a })
              (First' { getFirst' = Only _ }) =
              (First' { getFirst' = Only a })

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =  First' String
                  -> First' String
                  -> First' String
                  -> Bool

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
 x <- arbitrary
 return First' { getFirst' = x }

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst


main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)

import Data.Monoid (mempty, mappend)

-- 8. Mem
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem f') =
    Mem $ \s -> let (firstA, firstS) = f' s
                    (secondA, secondS) = f firstS in
                    (mappend firstA secondA, secondS)

f' = Mem $ \s -> ("hi", s + 1)

checkMem = do
  let
      -- We give runMem the neutral element (mempty) of the monoid Mem,
      -- then we fix Mem's first type variable s to Integer using 0.
      -- runMem is now (a -> (a, Integer)) meaning Mem's a is still unbound,
      -- but according to Mem's monoid instance that a has to have a
      -- monoid instance, too. Since we passed Mem's mempty to runMem,
      -- the value of a will be a's mempty; that's how mempty is
      -- defined in Mem's monoid instance.
      -- Later, with rmzero :: (String, Int), we fix a to String. String's
      -- mempty is "", yielding ("",0)
      rmzero  = runMem mempty 0
      rmleft  = runMem (f' `mappend` mempty) 0
      rmright = runMem (mempty `mappend` f') 0
  -- ("hi",1)
  print $ rmleft
  -- ("hi",1)
  print $ rmright
  -- ("",0)
  print (rmzero :: (String, Int))
  -- True
  print $ rmleft == runMem f' 0
  -- True
  print $ rmright == runMem f' 0

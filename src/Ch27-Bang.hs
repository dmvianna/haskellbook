{-# LANGUAGE BangPatterns #-}

module ManualBang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

banging :: Bool -> Int
banging !b = 1

data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y

data DoesntForce = TisLazy Int String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

data BangBang = SheShotMeDown !Int !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s

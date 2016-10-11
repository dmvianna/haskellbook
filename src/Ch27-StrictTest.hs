{-# LANGUAGE Strict #-}

module StrictTest where

blah x = 1

main = print (blah undefined)



{-# OPTIONS_GHC -F -pgmF ./something_nasty #-}
module B where

{-# INCLUDE C.hs #-}

x = 11

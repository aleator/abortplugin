{-#LANGUAGE CPP#-}
{-#LANGUAGE TypeFamilies#-}
module Imports where
import Data.List
import Data.Ord
import Data.Functor
import A
y=15

class Foo a where
    type K a :: *
    f :: K a -> a

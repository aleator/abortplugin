{-#LANGUAGE DeriveGeneric,DeriveAnyClass #-}
module AbortPlugin.Limitations where

import GHC.Generics(Generic)
import qualified Data.Text
import qualified Dhall

-- <# Limitations data type#>
data Limitations = Limitations
        { extensions :: [Data.Text.Text]
        , imports    :: [Data.Text.Text]}
        deriving (Generic,Dhall.Interpret, Dhall.Inject, Show)


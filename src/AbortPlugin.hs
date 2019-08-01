{-#LANGUAGE DeriveGeneric, DeriveAnyClass#-}
{-#OPTIONS_GHC -Wall#-}
module AbortPlugin (plugin) where

import Control.Monad
import Control.Monad.IO.Class

import GHC.Generics

import qualified EnumSet as S
import           GhcPlugins hiding (errorMsg,(<>))
import           ErrUtils
import           GHC.Generics()
import           Data.List ((\\))
import Control.Exception (throw)

import qualified Bag
import qualified HscTypes
import qualified Dhall

-- <# Limitations data type#>
data Limitations = Limitations {extensions :: [String], imports ::[String]} deriving (Generic,Dhall.Interpret,Show)

-- <# Main #>
plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \commandLineOptions modSummary parsedModule -> do
                           case commandLineOptions of 
                            [x] -> do
                                    limitations <- liftIO (Dhall.inputFile Dhall.auto x :: IO Limitations)
                                    importErrors <- checkImports (imports limitations) modSummary
                                    extensionErrors <- checkExts (AbortPlugin.extensions limitations) parsedModule
                                    unless (null extensionErrors && null importErrors) $ 
                                     liftIO (throw (HscTypes.mkSrcErr (Bag.listToBag (extensionErrors ++ importErrors))))
                                    pure parsedModule
                            [] -> error "Please support a limitations.dhall as argument"
                            _  -> error "This plugin takes one argument that is a dhall file of type "
                -- TODO: Error causes a ghc-panic...
  }

-- <# Extension Checking #> 
checkExts :: (HasDynFlags m, MonadIO m) => [String] -> HsParsedModule -> m [ErrMsg]
checkExts allowed (HsParsedModule { hpm_module = L pos _ })
  = do
    flags <- getDynFlags

    let 
        extensionsUsed    = extensionFlags flags
        badExtensions = map show (S.toList extensionsUsed) \\  allowed
        extensionErrors =
         [ ErrUtils.mkPlainErrMsg
            flags
            pos
            (ErrUtils.formatErrDoc
              flags
              (ErrUtils.errDoc
                [ text ("Forbidden extension used: " ++ show extensionName) ]
                []
                []
              )
            )
         | extensionName <- badExtensions
         ]
    pure (extensionErrors)
    --liftIO (throw (HscTypes.mkSrcErr (Bag.listToBag extensionErrors)))
    --unless (null badExtensions) $ liftIO (ghcExit flags 1)

-- <# Import Checking #>
checkImports
  :: (HasDynFlags m, MonadIO m)
  => [String]
  -> ModSummary
  -> m [ErrMsg]
checkImports allowedImports modSummary 
  = do
    flags <- getDynFlags

    let
      importsUsed =
        map snd (ms_textual_imps modSummary <> ms_srcimps modSummary)
      badImports =
        [ rval
        | rval@(L _ n) <- importsUsed
        , moduleNameString n `notElem` allowedImports
        ]
      importErrors =
        [ ErrUtils.mkPlainErrMsg
            flags
            importPosition
            (ErrUtils.formatErrDoc
              flags
              (ErrUtils.errDoc
                [ text
                    ("Forbidden import used: " ++ moduleNameString importName)
                ]
                []
                []
              )
            )
        | L importPosition importName <- badImports
        ]
    pure (importErrors)

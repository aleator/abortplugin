{-#LANGUAGE DeriveGeneric, StandaloneDeriving#-}
module AbortPlugin (plugin) where

import Control.Monad
import Control.Monad.IO.Class

import qualified EnumSet as S
import           GhcPlugins hiding (errorMsg,(<>))
import           ErrUtils
import           Data.Traversable
import           GHC.LanguageExtensions
import           GHC.Generics
import           Data.List (foldl',(\\))
import           Data.Monoid
import           Data.Data
import Control.Exception (throw)

import qualified Bag
import qualified HscTypes
import HsSyn
import ApiAnnotation
import HeaderInfo
import qualified Dhall


plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \commandLineOptions modSummary parsedModule -> do
                           checkExts    commandLineOptions modSummary parsedModule
                           checkImports commandLineOptions modSummary parsedModule
                           pure parsedModule
  }

deriving instance Typeable DynFlags

checkExts :: (HasDynFlags m, MonadIO m) => [CommandLineOption] -> ModSummary -> HsParsedModule -> m ()
checkExts commandLineOptions modSummary (HsParsedModule { hpm_module = L pos parsedModule, hpm_src_files = src_files, hpm_annotations = (annKeys, annotations) })
  = do
    flags <- getDynFlags
    let allowed :: [Extension]
        allowed =
          [ MonomorphismRestriction
          , RelaxedPolyRec
          , ForeignFunctionInterface
          , ImplicitPrelude
          , DoAndIfThenElse
          , EmptyDataDecls
          , PatternGuards
          , NondecreasingIndentation
          , TraditionalRecordSyntax
          , MonadFailDesugaring
          , StarIsType
          ]
        HsModule { hsmodName = name, hsmodExports = exports, hsmodImports = imports, hsmodDecls = decls }
         = parsedModule

        extensions    = extensionFlags flags
        badExtensions = S.toList (foldl' (flip S.delete) extensions allowed)

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
    liftIO (throw (HscTypes.mkSrcErr (Bag.listToBag extensionErrors)))
    unless (null badExtensions) $ liftIO (ghcExit flags 1)

checkImports :: (HasDynFlags m, MonadIO m) => [CommandLineOption] -> ModSummary -> HsParsedModule -> m ()
checkImports commandLineOptions modSummary (HsParsedModule { hpm_module = L pos parsedModule, hpm_src_files = src_files, hpm_annotations = (annKeys, annotations) })
  = do
    flags <- getDynFlags
    let allowedImports :: [String]
        allowedImports = "Prelude" : ["Data.List"]
        HsModule { hsmodName = name, hsmodExports = exports, hsmodImports = imports, hsmodDecls = decls }
          = parsedModule

        importsUsed = map snd (ms_textual_imps modSummary <> ms_srcimps modSummary)
        badImports =
          [ rval
          | rval@(L _ n) <- importsUsed
          , moduleNameString n `notElem` allowedImports
          ]

    let
      importErrors =
        [ ErrUtils.mkPlainErrMsg
            flags
            importPosition
            (ErrUtils.formatErrDoc
              flags
              (ErrUtils.errDoc
                [ text ("Forbidden import used: " ++ moduleNameString importName) ]
                []
                []
              )
            )
        | L importPosition importName <- badImports
        ]
    liftIO (throw (HscTypes.mkSrcErr (Bag.listToBag importErrors)))

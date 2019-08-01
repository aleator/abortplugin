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


plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_ modSummary parsedModule ->
                           parsedModule <$ checkExts modSummary parsedModule
  }

deriving instance Typeable DynFlags

checkExts :: (HasDynFlags m, MonadIO m) => ModSummary -> HsParsedModule -> m ()
checkExts modSummary (HsParsedModule { hpm_module = L pos parsedModule, hpm_src_files = src_files, hpm_annotations = (annKeys, annotations) })
  = do
    flags <- getDynFlags
    let allowedImports :: [String]
        allowedImports = "Prelude" : ["Data.List"]
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
    -- let pppr = showSDoc flags . ppr

    let
      HsModule { hsmodName = name, hsmodExports = exports, hsmodImports = imports, hsmodDecls = decls }
        = parsedModule
      cachedFlags = ms_hspp_opts modSummary

    let imports = map snd (ms_textual_imps modSummary <> ms_srcimps modSummary)
        badImports =
          [ rval
          | rval@(L _ n) <- imports
          , moduleNameString n `notElem` allowedImports
          ]
    liftIO $ putStrLn (showSDoc flags (ppr decls))
    let extensions = extensionFlags flags
        bad        = S.toList (foldl' (flip S.delete) extensions allowed)
    let currentSettings = settings flags
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


    unless (null bad) $ liftIO $ do
      errorMsg flags $ mkLocMessage
        SevFatal
        pos
        (text ("Forbidden extension used: " ++ show bad))
      ghcExit flags 2

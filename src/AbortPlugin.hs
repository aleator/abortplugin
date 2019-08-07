{-#LANGUAGE DeriveGeneric, DeriveAnyClass#-}
{-#OPTIONS_GHC -Wall#-}
module AbortPlugin
  ( plugin
  )
where

import           Control.Monad
import           Control.Monad.IO.Class

import qualified EnumSet                       as S
import           GhcPlugins              hiding ( errorMsg
                                                , (<>)
                                                )
import           ErrUtils
import           Data.List                      ( (\\) )
import           Control.Exception              ( throw )

import qualified GHC.LanguageExtensions
import qualified Bag
import qualified HscTypes
import qualified Dhall
import qualified Data.Text
import qualified Dhall.Pretty

import AbortPlugin.Limitations

-- <# Main #>
plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction =
    \commandLineOptions modSummary parsedModule -> do
      case commandLineOptions of
        ['+' : x] -> do
          let importsUsed = getImports modSummary
          extensionsUsed <- getExtensions
          let limitations = Limitations
                { AbortPlugin.Limitations.extensions = map (Data.Text.pack . show)
                                               extensionsUsed
                , imports = fmap (Data.Text.pack . moduleNameString . unLoc)
                                 importsUsed
                }
          liftIO
            . writeFile x
            . show
            . Dhall.Pretty.prettyExpr
            . Dhall.embed Dhall.inject
            $ limitations
          pure parsedModule

        [x] -> do
          limitations <- liftIO (Dhall.inputFile Dhall.auto x :: IO Limitations)
          importErrors <- checkImports
            (fmap Data.Text.unpack (imports limitations))
            modSummary
          extensionErrors <- checkExts
            (fmap Data.Text.unpack (AbortPlugin.Limitations.extensions limitations))
            parsedModule
          unless (null extensionErrors && null importErrors) $ liftIO
            (throw
              (HscTypes.mkSrcErr
                (Bag.listToBag (extensionErrors ++ importErrors))
              )
            )
          pure parsedModule
        [] -> error "Please support a limitations.dhall as argument"
        _ ->
          error "This plugin takes one argument that is a dhall file of type "
                -- TODO: Error causes a ghc-panic...
  }

-- <# Extension Checking #> 
getExtensions
  :: (HasDynFlags m, MonadIO m) => m [GHC.LanguageExtensions.Extension]
getExtensions = S.toList . extensionFlags <$> getDynFlags

checkExts
  :: (HasDynFlags m, MonadIO m) => [String] -> HsParsedModule -> m [ErrMsg]
checkExts allowed (HsParsedModule { hpm_module = L pos _ }) = do
  flags          <- getDynFlags
  extensionsUsed <- getExtensions
  let badExtensions = map show extensionsUsed \\ allowed
      extensionErrors =
        [ ErrUtils.mkPlainErrMsg
            flags
            pos
            (ErrUtils.formatErrDoc
              flags
              (ErrUtils.errDoc
                [text ("Forbidden extension used: " ++ show extensionName)]
                []
                []
              )
            )
        | extensionName <- badExtensions
        ]
  pure (extensionErrors)



-- <# Import Checking #>
getImports :: ModSummary -> [GenLocated SrcSpan ModuleName]
getImports modSummary =
  map snd (ms_textual_imps modSummary <> ms_srcimps modSummary)

checkImports
  :: (HasDynFlags m, MonadIO m) => [String] -> ModSummary -> m [ErrMsg]
checkImports allowedImports modSummary = do
  flags <- getDynFlags
  let importsUsed = getImports modSummary

  let
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
              [text ("Forbidden import used: " ++ moduleNameString importName)]
              []
              []
            )
          )
      | L importPosition importName <- badImports
      ]
  pure (importErrors)

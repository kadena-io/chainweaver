{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
-- Necessary to have ghci reload that file on SASS file changes:
{-# OPTIONS_GHC -fforce-recomp #-}


-- | Template Haskell for "Frontend".
-- Copyright   :  (C) 2018 Kadena
-- License     :  BSD-style (see the file LICENSE)
--

module Frontend.TH where

import Language.Haskell.TH (Q, Exp(LitE), Lit (StringL))

-- C library in use, luckily we don't need it in the ghcjs build.
#ifdef  ghcjs_HOST_OS

renderCss :: Q Exp
renderCss = pure $ LitE $ StringL $ "Head is supposed to be rendered statically and not in ghcjs!"

#else
import TH.RelativePaths (withCabalPackageWorkDir)
import qualified Text.Sass as Sass
import Data.Default
import Control.Monad.IO.Class (liftIO)

-- | Render our SASS files to CSS.
--
--   This only works on GHC, not on GHCJS which is fine as we render the head
--   statically.
renderCss :: Q Exp
renderCss = do
    css <- either show Sass.resultString <$> sassCompile
    pure $ LitE $ StringL css
  where
    sassCompile = withCabalPackageWorkDir $
      liftIO $ Sass.compileFile "sass/index.scss" def
#endif

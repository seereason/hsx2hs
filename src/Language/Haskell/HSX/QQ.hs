{- | this module provides a QuasiQuoter that supports the HSX syntax.

-- Module      :  Language.Haskell.HSX.Tranform
-- Copyright   :  (c) Niklas Broberg 2004-2012
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--

You will need to enable the QuasiQuotes extension for it to work, which you can do by adding this to the top of your file:

    {&#45;\# LANGUAGE QuasiQuotes \#&#45;}

Here is a simple example that generates an HTML fragment:

> import Data.Char        (toUpper)
> import HSX.QQ           (hsx)
> import HSX.XMLGenerator
>
> html :: (XMLGenerator m) => XMLGenT m (XMLType m)
> html = [hsx| <p><% map toUpper "hello, world!"  %></p> |]

The syntax used by the hsx QuasiQuoter is the same as what is used by
@trhsx@. It is mostly normal XML syntax which a few key differences:

 1. strings inside tags and attributes are automatically escaped -- you do not need to do &lt;, etc.

 2. The <% %> syntax is used to embed the result of evaluating a Haskell expression into the XML

Values are embedde using the 'EmbedAsChild' and 'EmbedAsAttr'
classes. Additional instances can be added to support application
specific types.

-}
module Language.Haskell.HSX.QQ
    ( hsx
    )
    where


import qualified Language.Haskell.Exts.Syntax           as Hs
import           Language.Haskell.Exts                  hiding (Exp, parse, parseExp)
import           Language.Haskell.HSX.Transform         (transformExp)
import           Language.Haskell.Meta.Parse            hiding (parseHsExp, parseExp)
import           Language.Haskell.Meta.Syntax.Translate (toExp)
import           Language.Haskell.TH                    (Exp, ExpQ)
import           Language.Haskell.TH.Quote              (QuasiQuoter(..))

-- | QuasiQuoter which can be used to parse HSX syntax
hsx :: QuasiQuoter
hsx = QuasiQuoter { quoteExp  = parseHsxExp
                  , quotePat  = error "the hsx QuasiQuoter can only be used on expressions."
                  , quoteType = error "the hsx QuasiQuoter can only be used on expressions."
                  , quoteDec  = error "the hsx QuasiQuoter can only be used on expressions."
                  }

parseHsxExp :: String -> ExpQ
parseHsxExp = either (error . show) (return . toExp . transformExp) . parseHsExp

parseExp :: String -> Either String Exp
parseExp = either Left (Right . toExp . transformExp) . parseHsExp

parseHsExp :: String -> Either String Hs.Exp
parseHsExp = either Left (Right . transformExp) . parseResultToEither . parseExpWithMode parseMode

parseMode :: ParseMode
parseMode = ParseMode "" Haskell2010 allExtensions False True (Just baseFixities)
#if MIN_VERSION_haskell_src_exts(1,17,0)
                      False
#endif

allExtensions :: [Extension]
allExtensions = map EnableExtension
    [RecursiveDo,ParallelListComp,MultiParamTypeClasses,FunctionalDependencies,RankNTypes,ExistentialQuantification,
     ScopedTypeVariables,ImplicitParams,FlexibleContexts,FlexibleInstances,EmptyDataDecls,KindSignatures,
     BangPatterns,TemplateHaskell,ForeignFunctionInterface,Arrows,Generics,NamedFieldPuns,PatternGuards,
     MagicHash,TypeFamilies,StandaloneDeriving,TypeOperators,RecordWildCards,GADTs,UnboxedTuples,
     PackageImports,QuasiQuotes,TransformListComp,ViewPatterns,XmlSyntax,RegularPatterns]

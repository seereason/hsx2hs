{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.XMLGenerator
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, nibro@cs.chalmers.se
-- Stability   :  experimental
-- Portability :  requires newtype deriving and MPTCs with fundeps
--
-- The class and monad transformer that forms the basis of the literal XML
-- syntax translation. Literal tags will be translated into functions of
-- the GenerateXML class, and any instantiating monads with associated XML
-- types can benefit from that syntax.
-----------------------------------------------------------------------------
module HSX.XMLGenerator where

import Control.Monad.Trans

----------------------------------------------
-- General XML Generation

-- | The monad transformer that allows a monad to generate XML values.
newtype XMLGenerator m a = XMLGenerator (m a)
  deriving (Monad, Functor, MonadIO)

-- | un-lift.
unXMLGenerator :: XMLGenerator m a -> m a
unXMLGenerator (XMLGenerator ma) = ma

instance MonadTrans XMLGenerator where
 lift = XMLGenerator

-- | Generate XML values in some XMLGenerator monad.
class GenerateXML m xml attr child | m -> xml attr child where
 genElement  :: (Maybe String, String) -> [attr] -> [child] -> XMLGenerator m xml
 genEElement :: (Maybe String, String) -> [attr]            -> XMLGenerator m xml
 genEElement n ats = genElement n ats []

-- | Embed values as child nodes of an XML element. The parent type will be clear
-- from the context so it is not mentioned.
class EmbedAsChild a child where
 asChild :: a -> child

-- | Similarly embed values as attributes of an XML element.
class EmbedAsAttr a attr where
 asAttr :: a -> attr


-- literally lifted from the HList library
class TypeCast   a b   | a -> b, b -> a      where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a  where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a  where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b   where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

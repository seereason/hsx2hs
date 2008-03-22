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
newtype XMLGenT m a = XMLGenT (m a)
  deriving (Monad, Functor, MonadIO)

-- | un-lift.
unXMLGenT :: XMLGenT m a -> m a
unXMLGenT   (XMLGenT ma) =  ma

instance MonadTrans XMLGenT where
 lift = XMLGenT

-- | Generate XML values in some XMLGenerator monad.
class XMLGenerator m where
 type XML m
 type Child m
 type Attribute m
 genElement  :: (Maybe String, String) -> [Attribute m] -> [Child m] -> XMLGenT m (XML m)
 genEElement :: (Maybe String, String) -> [Attribute m]              -> XMLGenT m (XML m)
 genEElement n ats = genElement n ats []

-- | Embed values as child nodes of an XML element. The parent type will be clear
-- from the context so it is not mentioned.
class EmbedAsChild a c where
 asChild :: a -> c

-- | Similarly embed values as attributes of an XML element.
class EmbedAsAttr a at where
 asAttr :: a -> at

data Attr n a = n := a
  deriving (Show)


-- literally lifted from the HList library
class TypeCast   a b   | a -> b, b -> a      where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a  where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a  where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b   where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

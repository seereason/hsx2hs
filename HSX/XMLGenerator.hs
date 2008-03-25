-----------------------------------------------------------------------------
-- |
-- Module      :  HSX.XMLGenerator
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
import Control.Monad (liftM)

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

type Name = (Maybe String, String)

-- | Generate XML values in some XMLGenerator monad.
class Monad m => XMLGenerator m where
 type XML m
 type Child m
 type Attribute m
 genElement  :: Name -> [XMLGenT m (Attribute m)] -> [XMLGenT m [Child m]] -> XMLGenT m (XML m)
 genEElement :: Name -> [XMLGenT m (Attribute m)]                          -> XMLGenT m (XML m)
 genEElement n ats = genElement n ats []

-- | Embed values as child nodes of an XML element. The parent type will be clear
-- from the context so it is not mentioned.
class EmbedAsChild a c where
 asChild :: a -> c

-- | Similarly embed values as attributes of an XML element.
class EmbedAsAttr a at where
 asAttr :: a -> at

data Attr n a = n := a
  deriving Show


-------------------------------------
-- Setting attributes

-- | Set attributes on XML elements
class XMLGenerator m => SetAttr m t where
 setAttr :: t -> XMLGenT m (Attribute m) -> XMLGenT m (XML m)
 setAll  :: t -> XMLGenT m [Attribute m] -> XMLGenT m (XML m)
 setAttr t v = setAll t $ liftM return v

(<@), set :: (SetAttr m t, EmbedAsAttr a (XMLGenT m (Attribute m))) => t -> a -> XMLGenT m (XML m)
set xml at = setAttr xml (asAttr at)
(<@) = set

(<<@) :: (SetAttr m t, EmbedAsAttr a (XMLGenT m (Attribute m))) => t -> [a] -> XMLGenT m (XML m)
xml <<@ ats = setAll xml (mapM asAttr ats)

-------------------------------------
-- Appending children

class XMLGenerator m => AppendChild m t where
 appChild :: t -> XMLGenT m (Child m) -> XMLGenT m (XML m)
 appAll   :: t -> XMLGenT m [Child m] -> XMLGenT m (XML m)
 appChild t c = appAll t $ liftM return c

(<:), app :: (AppendChild m t, EmbedAsChild c (XMLGenT m [Child m])) => t -> c -> XMLGenT m (XML m)
app t c = appAll t $ asChild c
(<:) = app

-------------------------------------
-- Names

-- | Names can be simple or qualified with a domain. We want to conveniently
-- use both simple strings or pairs wherever a Name is expected.
class Show n => IsName n where
 toName :: n -> Name

-- | Names can represent names, of course.
instance IsName Name where
 toName = id

-- | Strings can represent names, meaning a simple name with no domain.
instance IsName String where
 toName s = (Nothing, s)

-- | Pairs of strings can represent names, meaning a name qualified with a domain.
instance IsName (String, String) where
 toName (ns, s) = (Just ns, s)


-- literally lifted from the HList library
class TypeCast   a b   | a -> b, b -> a      where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a  where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a  where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b   where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

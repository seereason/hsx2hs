{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}
{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module HSX.Layout (
        Layout(..), IsLayout(..),
        above, beside, (^^), (<>)
    )where

import Prelude hiding ((^^))

import HSX.XMLGenerator

data Layoutm
  = Above (Layout m) (Layout m)
  | Beside (Layout m) (Layoutm)
  | Item (XMLGenT m [Child m])

class IsLayout m a where
 toLayout :: a -> Layout m


instance IsLayout m (Layout m) where
 toLayout = id

instance (EmbedAsChild m c) => IsLayout m c where
 toLayout = Item . asChild

(^^),(<>),above,beside :: (IsLayout m a, IsLayout m b) =>a -> b -> Layout m
a ^^ b = Above  (toLayout a) (toLayout b)
a <> b = Beside (toLayout a) (toLayout b)
above = (^^)
beside = (<>)


instance (XMLGenerator m, 
          EmbedAsChild m (XMLGenT m (XML m)), 
          EmbedAsChild m (XMLGenT m [XML m]),
          EmbedAsAttr  m (Attr String String))
          => EmbedAsChild m (Layout m) where
 asChild a@(Above _ _) = asChild (
   <table border="0"><% 
       mapM mkRow $ foldAbove a :: XMLGenT m [XML m]
   %></table> :: XMLGenT m (XML m))
 asChild b@(Beside _ _) = asChild (
   <table border="0"><%
     <tr><% 
       mapM mkCell $ foldBeside b :: XMLGenT m [XML m] 
     %></tr>  :: XMLGenT m (XML m)
   %></table> :: XMLGenT m (XML m))
 asChild (Item xml) = xml

foldAbove :: Layout m -> [Layout m]
foldAbove (Above a b) = foldAbove a ++ foldAbove b
foldAbove l = [l]

foldBeside :: Layout m -> [Layout m]
foldBeside (Beside a b) = foldBeside a ++ foldBeside b
foldBeside l = [l]

mkRow :: forall m c . (XMLGenerator m, 
                       EmbedAsChild m c, 
                       EmbedAsChild m (XMLGenT m (XML m))) 
       => c -> XMLGenT m (XML m)
mkRow xml = <tr><% (mkCell xml :: XMLGenT m (XML m)) %></tr> 

mkCell :: (XMLGenerator m, EmbedAsChild m c) => c -> XMLGenT m (XML m)
mkCell xml = <td><% xml %></td>

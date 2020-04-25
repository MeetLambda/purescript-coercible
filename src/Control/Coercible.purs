module Control.Coercible
  ( class Coercible
  , coerce ) where

import Control.Bind (class Applicative, class Bind, join, pure)
import Control.Comonad (class Comonad, extract)
import Data.Foldable (foldMap)
import Data.Function (const, identity, (<<<))
import Data.Functor (class Functor, map)
import Data.Int (toNumber)
import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.CodeUnits (fromCharArray, toCharArray, uncons)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)

class Coercible a b where
  coerce :: a -> b

instance coercibleUnit :: Coercible a Unit where
  coerce _ = unit
else
instance coercibleVoid :: Coercible Void a where
  coerce = absurd
else
instance coercibleImage :: Coercible a (b -> a) where
  coerce = const
else
instance coercibleIntNumber :: Coercible Int Number where
  coerce = toNumber
else
instance coercibleCharString :: Coercible Char String where
  coerce = fromChar
else
instance coercibleArrayCharString :: Coercible (Array Char) String where
  coerce = fromCharArray
else
instance coercibleStringArrayChar :: Coercible String (Array Char) where
  coerce = toCharArray
else
instance coercibleFunctor :: (Functor f, Coercible a b) => Coercible (f a) (f b) where
  coerce = map coerce
else
instance coercibleF :: (Functor f, Newtype b a) => Coercible (a -> f a) (b -> f b) where
    coerce fa = \bt -> map wrap (fa (unwrap bt))
else
instance coercibleApplicative :: Applicative f => Coercible a (f a) where
  coerce = pure
else
instance coercibleBind :: Bind m => Coercible (m (m a)) (m a) where
  coerce = join
else
instance coercibleComonad :: Comonad m => Coercible (m a) a where
  coerce = extract
else
instance coercableListCharString :: Coercible (List Char) String where
  coerce = foldMap fromChar
else
instance coercibleStringListChar :: Coercible String (List Char) where
  coerce = reverse <<< coerce' Nil where
    coerce' acc str =
      case uncons str of
           Just { head, tail } -> coerce' (head : acc) str
           _ -> acc
-- else
-- instance coercibleId :: Coercible a a where
--   coerce = identity

fromChar :: Char -> String
fromChar c = fromCharArray [c]
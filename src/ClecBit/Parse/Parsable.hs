{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ClecBit.Parse.Parsable
  ( Parsable(..)
  ) where

import Data.Bifunctor (bimap)
import Data.Text (Text)
import qualified Data.Text.Read as TR
import qualified Data.Traversable as T
import qualified HBet.Types as TY

class Parsable from to where
  parseData :: from -> Either String to

instance Parsable a a where
  parseData = Right

instance Parsable Text Int where
  parseData t = TR.decimal t >>= (\(r, _) -> Right r)

instance (Parsable a b, Traversable t) => Parsable (t a) (t b) where
  parseData f = T.sequenceA $ fmap parseData f

instance (Parsable a b, Parsable c d) => Parsable (a, c) (b, d) where
  parseData (a, b) = (,) <$> parseData a <*> parseData b

instance (Parsable a x, Parsable a y) => Parsable a (x, y) where
  parseData a = (,) <$> parseData a <*> parseData a

instance Parsable Text TY.WinOrDraw where
  parseData t =
    case t of
      "%1%" -> return TY.W1
      "%N%" -> return TY.Draw
      "%2%" -> return TY.W2
      _ -> Left $ "Unknown win or draw: " ++ show t

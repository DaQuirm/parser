{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Uncons where

import Data.Text (Text)
import qualified Data.Text as Text

class Uncons s c | s -> c where
  uncons :: s -> Maybe (c, s)

instance Uncons [a] a where
  uncons []      = Nothing
  uncons (x: xs) = Just (x, xs)

instance Uncons Text Char where
  uncons = Text.uncons

instance Uncons Int Int where
  uncons n = Just (n, n)

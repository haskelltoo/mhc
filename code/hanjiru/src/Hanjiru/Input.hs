{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ViewPatterns           #-}

module Hanjiru.Input where

import Prelude
import Data.Functor.Identity

import Data.Text      qualified
import Data.Text.Lazy qualified

-- | Instances of 'Input' define the types of inputs that can be parsed character-by-character
--   like prototypical strings.

class Input input char | input -> char where

  uncons :: input -> Maybe (char, input)

instance Input [elt] elt where

  uncons []     = Nothing
  uncons (x:xs) = Just (x, xs)
  {-# INLINE uncons #-}

instance Input Data.Text.Text Char where

  uncons = Data.Text.uncons
  {-# INLINE uncons #-}

instance Input Data.Text.Lazy.Text Char where

  uncons = Data.Text.Lazy.uncons
  {-# INLINE uncons #-}

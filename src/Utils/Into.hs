{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.Into where

class Into a b where
  into :: a -> b

instance (Into a b) => Into [a] [b] where
  into = map into

instance (Into a b) => Into (Maybe a) (Maybe b) where
  into = fmap into

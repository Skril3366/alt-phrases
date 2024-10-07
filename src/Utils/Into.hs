{-# LANGUAGE MultiParamTypeClasses #-}

module Utils.Into where

class Into a b where
  into :: a -> b

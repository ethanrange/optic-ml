{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module OpticML.Parametric
  ( Para(..)
  )
where

import OpticML.Lenses (Lens)

data Para p s t a b = Para
    { params :: p
    , plens :: Lens s t a b
    }

composePara :: Para p s t a b -> Para q a b m n -> Para r s t m n
composePara = undefined
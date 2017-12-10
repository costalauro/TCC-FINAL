{-# LANGUAGE TemplateHaskell #-}

module TipoTreinamento where
 
import Database.Persist.TH
import Prelude

data TipoTreinamento =  Aberto | Fechado deriving (Show, Eq, Read)

derivePersistField "TipoTreinamento"
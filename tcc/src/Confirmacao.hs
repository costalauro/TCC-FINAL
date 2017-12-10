{-# LANGUAGE TemplateHaskell #-}

module Confirmacao where
 
import Database.Persist.TH
import Prelude

data Confirmacao = Confirmada | NaoConfirmada deriving (Show, Eq, Read)

derivePersistField "Confirmacao"
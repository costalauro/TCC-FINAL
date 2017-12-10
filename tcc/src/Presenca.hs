{-# LANGUAGE TemplateHaskell #-}

module Presenca where
 
import Database.Persist.TH
import Prelude

data Presenca =  Obrigatoria | NaoObrigatoria deriving (Show, Eq, Read)

derivePersistField "Presenca"
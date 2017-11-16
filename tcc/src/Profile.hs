{-# LANGUAGE TemplateHaskell #-}

module Profile where
 
import Database.Persist.TH
import Prelude

data Profile =  Responsavel | Funcionario deriving (Show, Eq, Read)

derivePersistField "Profile"
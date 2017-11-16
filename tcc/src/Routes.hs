{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Routes where
import Yesod
 
pRoutes = [parseRoutes|
    / HomeR GET
    /static StaticR Static getStatic
    /login LoginR GET
|]
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}

module Handlers where
import Routes
import Yesod
import Tools
import Database.Persist.Postgresql
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Time
import qualified Data.Text as T
import Text.Julius
import Text.Lucius  
import Text.Hamlet
import Text.Cassius 
import Profile
import Yesod.Form.Jquery
import Yesod.Static
import Network.Mail.Mime
import Database.Persist.Postgresql

mkYesodDispatch "Sistreina" pRoutes

loginForm :: Form (Text,Text)
loginForm = renderDivs $ (,) <$>
           areq textField (fieldSettingsLabel MsgTxtLogin) Nothing <*>
           areq passwordField (fieldSettingsLabel MsgTxtSenha) Nothing
           
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Sistreina" 
        indexWidget $(whamletFile "view/whamlet/home.hamlet") 
        
getLoginR :: Handler Html
getLoginR = do
        (widget, enctype) <- generateFormPost loginForm
        defaultLayout $ do 
        setTitle "Sistreina" 
        indexWidget $(whamletFile "view/whamlet/login.hamlet")    
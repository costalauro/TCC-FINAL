{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Tools where
import Routes
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Text.Julius
import Text.Lucius  
import Text.Hamlet
import Text.Cassius 
import Network.Mail.Mime 
import Network.Mail.SMTP 
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Text.Lazy.Encoding 

logWid :: Widget
logWid = [whamlet| 
    Login
|]
        
coreWidget = do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css  
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "view/hamlet/head.hamlet")
            toWidget $(luciusFile "view/lucius/core.lucius") 

indexWidget :: Widget -> Widget
indexWidget hamletWidget = do
            coreWidget
            $(whamletFile "view/widgets/header.hamlet") 
            hamletWidget
            $(whamletFile "view/widgets/footer.hamlet")  

respWidget :: Widget -> Widget
respWidget hamletWidget = do
            coreWidget 
            $(whamletFile "view/widgets/header.hamlet") 
            hamletWidget
            $(whamletFile "view/widgets/footer.hamlet")      
            
funcWidget :: Widget -> Widget
funcWidget hamletWidget = do
            coreWidget 
            $(whamletFile "view/widgets/header.hamlet") 
            hamletWidget
            $(whamletFile "view/widgets/footer.hamlet")
            
            
-- to generate generic forms

widgetForm :: Route Sistreina -> Enctype -> Widget -> Widget -> Widget 
widgetForm x enctype widget newWidget = [whamlet|
            <h1>
                ^{newWidget}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input ."btn btn-primary" type="reset"  #"limpar">
                <input ."btn btn-primary" type="submit"  #"cadastrar">
            <p>_{MsgCadastro}
|]
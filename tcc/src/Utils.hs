{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
             
module Utils where
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
    Entrar
|]
userWid :: Widget
userWid = [whamlet| 
    Cadastrar Usuário
|]
funcWid :: Widget
funcWid = [whamlet| 
    Cadastrar Funcionário
|]

deptWid :: Widget
deptWid = [whamlet| 
    Cadastrar Departamento
|]

profWid :: Widget
profWid = [whamlet| 
    Cadastrar Profissão
|]

treinWid :: Widget
treinWid = [whamlet| 
    Cadastrar Treinamento
|]

detWidget = do   
            toWidget $(juliusFile "templates/julius/detalhe.julius") 
            
masterWidget = do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(luciusFile "templates/lucius/principal.lucius")

customWidget :: Widget -> Widget
customWidget hamletWidget = do
            masterWidget
            $(whamletFile "templates/widgets/header.hamlet") 
            hamletWidget
            $(whamletFile "templates/widgets/footer.hamlet")  

respWidget :: Widget -> Widget
respWidget hamletWidget = do
            masterWidget
            $(whamletFile "templates/widgets/headerresp.hamlet") 
            hamletWidget
            $(whamletFile "templates/widgets/footer.hamlet") 
            
funcWidget :: Widget -> Widget
funcWidget hamletWidget = do
            masterWidget
            $(whamletFile "templates/widgets/headerfunc.hamlet") 
            hamletWidget
            $(whamletFile "templates/widgets/footer.hamlet") 

cadWidget = do    
            toWidget $(cassiusFile "templates/cassius/form.cassius")         

listWidget = do
            toWidget $(cassiusFile "templates/cassius/list.cassius") 
            
-- to generate generic forms

widgetForm :: Route Sistreina -> Enctype -> Widget -> Widget -> Widget 
widgetForm x enctype widget newWidget = [whamlet|
            <h1>
                ^{newWidget}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input ."btn btn-danger" type="reset"  #"limpar">
                <input ."btn btn-success" type="submit"  #"cadastrar">
            <p>_{MsgCadastro}
|]
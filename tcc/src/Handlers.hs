{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}

module Handlers where
import Routes
import Yesod
import Utils
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
           
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Sistreina" 
        customWidget $(whamletFile "templates/whamlet/home.hamlet")
        
getRespR :: Handler Html
getRespR = defaultLayout $ do
        setTitle "Sistreina" 
        respWidget $(whamletFile "templates/whamlet/home.hamlet")

getFuncR :: Handler Html
getFuncR = defaultLayout $ do
        setTitle "Sistreina" 
        funcWidget $(whamletFile "templates/whamlet/home.hamlet")
        
-- Usuario
usuarioForm :: Form Usuario
usuarioForm = renderDivs $ Usuario <$>   
       areq textField (fieldSettingsLabel MsgTxtNome) Nothing <*>  
       areq textField (fieldSettingsLabel MsgTxtEmail) Nothing <*>  
       areq passwordField (fieldSettingsLabel MsgTxtSenha) Nothing <*>
       areq (selectField $ optionsPairs [(MsgTxtResponsavellbl, Responsavel),(MsgTxtFuncionariolbl, Funcionario)]) (fieldSettingsLabel MsgForm4) Nothing
       
getCadUsuarioR :: Handler Html
getCadUsuarioR = do  
        (widget, enctype) <- generateFormPost usuarioForm
        defaultLayout $ do 
        setTitle "Sistreina -  Cadastro Usuario" 
        respWidget $(whamletFile "templates/whamlet/cadastro/cadUsuario.hamlet")    
        >> cadWidget
        
postCadUsuarioR :: Handler Html
postCadUsuarioR = do
           ((result, _), _) <- runFormPost usuarioForm
           case result of 
               FormSuccess user -> (runDB $ insert user) >> redirect SucessoR
               _ -> redirect ErroR
        
-- Login    
loginForm :: Form (Text,Text)
loginForm = renderDivs $ (,) <$>
           areq textField (fieldSettingsLabel MsgTxtLogin) Nothing <*>
           areq passwordField (fieldSettingsLabel MsgTxtSenha) Nothing
    
getLoginR :: Handler Html
getLoginR = do
        (widget, enctype) <- generateFormPost loginForm
        defaultLayout $ do 
        setTitle "Sistreina" 
        customWidget $(whamletFile "templates/whamlet/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost loginForm
           case result of  
                FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuarioNome ==. login, UsuarioSenha ==. senha] []
                   case user of  
                       Nothing -> redirect LoginR 
                       Just (Entity pid (Usuario nome login senha Responsavel)) ->  setSession "_ID" (pack $ show $ Responsavel) >> redirect RespR
                       Just (Entity pid (Usuario nome login senha Funcionario)) ->  setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect FuncR
                _ -> redirect ErroR  


getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout $ do
            setTitle "Sistreina" 
            customWidget $(whamletFile "templates/whamlet/logout.hamlet") 
            toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet") 
                           
func = do
       entidades <- runDB $ selectList [UsuarioTipo ==. Funcionario] [Asc UsuarioNome]
       optionsPairs $ fmap (\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades  

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

profs = do
       entidades <- runDB $ selectList [] [Asc ProfissaoNome] 
       optionsPairs $ fmap (\ent -> (profissaoSigla $ entityVal ent, entityKey ent)) entidades

treins = do
       entidades <- runDB $ selectList [] [Asc TreinamentoNome] 
       optionsPairs $ fmap (\ent -> (treinamentoNome $ entityVal ent, entityKey ent)) entidades
       
-- Funcionario

funcionarioForm :: Form Funcionarios
funcionarioForm = renderDivs $ Funcionarios <$>   
       areq textField (fieldSettingsLabel MsgTxtNome) Nothing <*>  
       areq intField "Idade :" Nothing <*>
       areq doubleField "Salário :" Nothing <*>
       areq dayField "Data Nascimento :" Nothing <*>
       areq (selectField dptos) "Departamentos :" Nothing <*>
       areq (selectField profs) "Profissoes :" Nothing
       
getCadFuncionarioR :: Handler Html
getCadFuncionarioR = do  
        (widget, enctype) <- generateFormPost funcionarioForm
        defaultLayout $ do 
        setTitle "Sistreina -  Cadastro Funcionario" 
        respWidget $(whamletFile "templates/whamlet/cadastro/cadFuncionario.hamlet")    
        >> cadWidget

postCadFuncionarioR :: Handler Html
postCadFuncionarioR = do
           ((result, _), _) <- runFormPost funcionarioForm
           case result of 
               FormSuccess func -> (runDB $ insert func) >> redirect SucessoR
               _ -> redirect ErroR

getListFuncionarioR :: Handler Html        
getListFuncionarioR = do 
        funcionarios <- runDB $ (rawSql "SELECT ??, ??,?? FROM funcionarios INNER JOIN departamento ON funcionarios.deptoid=departamento.id INNER JOIN profissao ON funcionarios.profid=profissao.id" [])::Handler [(Entity Funcionarios, Entity Departamento,Entity Profissao)] 
        defaultLayout $ do 
        setTitle "Sistreina - Lista Funcionarios" 
        respWidget $(whamletFile "templates/whamlet/listas/listFuncionarios.hamlet") 
        >> listWidget

getDetalheFuncionarioR :: FuncionariosId -> Handler Html
getDetalheFuncionarioR fid = do
      funcionario <- runDB $ get404 fid
      defaultLayout $ do 
            setTitle "Sistreina - Funcionario" 
            respWidget $(whamletFile "templates/whamlet/detalhe/funcionario.hamlet")   
            >> detWidget


postDetalheFuncionarioR :: FuncionariosId -> Handler Html
postDetalheFuncionarioR fid = do
     runDB $ delete fid
     redirect ListFuncionarioR
        
-- Departamento

departamentoForm :: Form Departamento
departamentoForm = renderDivs $ Departamento <$>   
       areq textField "Nome :" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","3")]} Nothing
                           
getCadDepartamentoR :: Handler Html
getCadDepartamentoR = do  
        (widget, enctype) <- generateFormPost departamentoForm
        defaultLayout $ do 
        setTitle "Sistreina -  Cadastro Departamento" 
        respWidget $(whamletFile "templates/whamlet/cadastro/cadDepartamento.hamlet")    
        >> cadWidget

postCadDepartamentoR :: Handler Html
postCadDepartamentoR = do
           ((result, _), _) <- runFormPost departamentoForm
           case result of 
               FormSuccess deptos -> (runDB $ insert deptos) >> redirect SucessoR
               _ -> redirect ErroR
               
getListDepartamentoR :: Handler Html        
getListDepartamentoR = do
        departamentos <- runDB $ selectList [] [Asc DepartamentoNome]
        defaultLayout $ do 
        setTitle "Sistreina - Lista de Departamento" 
        respWidget $(whamletFile "templates/whamlet/listas/listDepartamento.hamlet")      
        >> listWidget


getDetalheDepartamentoR :: DepartamentoId -> Handler Html
getDetalheDepartamentoR did = do
      departamento <- runDB $ get404 did
      defaultLayout $ do 
            setTitle "Sistreina - Departamento" 
            respWidget $(whamletFile "templates/whamlet/detalhe/departamento.hamlet")   
            >> detWidget


postDetalheDepartamentoR :: DepartamentoId -> Handler Html
postDetalheDepartamentoR did = do
     runDB $ delete did
     redirect ListDepartamentoR
     
-- Profissao

profissaoForm :: Form Profissao
profissaoForm = renderDivs $ Profissao <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Abreviação :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","10")]} Nothing <*>
            areq (selectField dptos) "Departamentos :" Nothing
            
getCadProfissaoR :: Handler Html
getCadProfissaoR = do  
        (widget, enctype) <- generateFormPost profissaoForm
        defaultLayout $ do 
        setTitle "Sistreina -  Cadastro Profissão" 
        respWidget $(whamletFile "templates/whamlet/cadastro/cadProfissao.hamlet")    
        >> cadWidget

postCadProfissaoR :: Handler Html
postCadProfissaoR = do
           ((result, _), _) <- runFormPost profissaoForm
           case result of 
               FormSuccess profs -> (runDB $ insert profs) >> redirect SucessoR
               _ -> redirect ErroR
        
getListProfissaoR :: Handler Html        
getListProfissaoR = do 
        profissoes <- runDB $ (rawSql "SELECT ??, ?? FROM profissao INNER JOIN departamento ON profissao.deptoid=departamento.id" [])::Handler [(Entity Profissao, Entity Departamento)] 
        defaultLayout $ do 
        setTitle "Sistreina - Lista Profissoes" 
        respWidget $(whamletFile "templates/whamlet/listas/listProfissao.hamlet") 
        >> listWidget

getDetalheProfissaoR :: ProfissaoId -> Handler Html
getDetalheProfissaoR pid = do
      profissao <- runDB $ get404 pid
      defaultLayout $ do 
            setTitle "Sistreina - Profissao" 
            respWidget $(whamletFile "templates/whamlet/detalhe/profissao.hamlet")   
            >> detWidget


postDetalheProfissaoR :: ProfissaoId -> Handler Html
postDetalheProfissaoR pid = do
     runDB $ delete pid
     redirect ListProfissaoR
    
-- Treinamento

treinamentoForm :: Form Treinamento
treinamentoForm = renderDivs $ Treinamento <$>
            areq textField "Nome :" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Abreviação :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","10")]} Nothing <*>
            areq textField "Responsavel :" Nothing <*>
            areq textField "Sala :" Nothing <*>
            areq intField "Qtd. Pessoas :" Nothing <*>
            areq (selectField profs) "Profissões :" Nothing <*>
            areq dayField "Data Aplicacao :" Nothing <*>
            areq dayField "Data Validade :" Nothing
            
getCadTreinamentoR :: Handler Html
getCadTreinamentoR = do  
        (widget, enctype) <- generateFormPost treinamentoForm
        defaultLayout $ do 
        setTitle "Sistreina -  Cadastro Treinamento" 
        respWidget $(whamletFile "templates/whamlet/cadastro/cadTreinamento.hamlet")    
        >> cadWidget

postCadTreinamentoR :: Handler Html
postCadTreinamentoR = do
           ((result, _), _) <- runFormPost treinamentoForm
           case result of 
               FormSuccess treins -> (runDB $ insert treins) >> redirect SucessoR
               _ -> redirect ErroR

getListTreinamentoR :: Handler Html        
getListTreinamentoR = do 
        treinamentos <- runDB $ (rawSql "SELECT ??, ?? FROM treinamento INNER JOIN profissao ON treinamento.profid=profissao.id" [])::Handler [(Entity Treinamento, Entity Profissao)] 
        defaultLayout $ do 
        setTitle "Sistreina - Lista Treinamentos" 
        respWidget $(whamletFile "templates/whamlet/listas/listTreinamento.hamlet") 
        >> listWidget
               
getDetalheTreinamentoR :: TreinamentoId -> Handler Html
getDetalheTreinamentoR tid = do
      treinamento <- runDB $ get404 tid
      defaultLayout $ do 
            setTitle "Sistreina - Treinamento" 
            respWidget $(whamletFile "templates/whamlet/detalhe/treinamento.hamlet")   
            >> detWidget

postDetalheTreinamentoR :: TreinamentoId -> Handler Html
postDetalheTreinamentoR tid = do
     runDB $ delete tid
     redirect ListTreinamentoR
     
getErroR :: Handler Html
getErroR = defaultLayout $ do  
        setTitle "Sistreina - Erro!" 
        respWidget $(whamletFile "templates/whamlet/error.hamlet")  
        toWidgetHead $(hamletFile "templates/hamlet/headresp.hamlet")

getErro2R :: Handler Html
getErro2R = defaultLayout $ do  
        setTitle "Sistreina - Erro!" 
        respWidget $(whamletFile "templates/whamlet/error.hamlet")  
        toWidgetHead $(hamletFile "templates/hamlet/headfunc.hamlet")

getSucessoR :: Handler Html
getSucessoR = defaultLayout $ do  
        setTitle "Sistreina - Sucesso!"  
        respWidget $(whamletFile "templates/whamlet/sucesso.hamlet") 
        toWidgetHead $(hamletFile "templates/hamlet/headresp.hamlet") 
        
getSucesso2R :: Handler Html
getSucesso2R = defaultLayout $ do  
        setTitle "Sistreina - Sucesso!"  
        respWidget $(whamletFile "templates/whamlet/sucesso.hamlet") 
        toWidgetHead $(hamletFile "templates/hamlet/headfunc.hamlet") 
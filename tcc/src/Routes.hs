{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Routes where
import Yesod
 
pRoutes = [parseRoutes|
    / HomeR GET
    /resp/ RespR GET
    /static StaticR Static getStatic
    /login LoginR GET POST
    /logout LogoutR GET
    
    /cad/funcionario/  CadFuncionarioR GET POST
    /cad/departamento/  CadDepartamentoR GET POST
    /cad/profissao/  CadProfissaoR GET POST
    /cad/treinamento/  CadTreinamentoR GET POST
    
    /list/departamento/ ListDepartamentoR GET
    /list/profissao/ ListProfissaoR GET
    /list/funcionario/ ListFuncionarioR GET
    
    /sucesso SucessoR GET
    /erro ErroR GET
|]
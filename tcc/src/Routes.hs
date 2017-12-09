{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Routes where
import Yesod
 
pRoutes = [parseRoutes|
    / HomeR GET
    /resp/ RespR GET
    /func/ FuncR GET
    /static StaticR Static getStatic
    /login LoginR GET POST
    /logout LogoutR GET
    
    /cad/usuario/  CadUsuarioR GET POST
    /cad/funcionario/  CadFuncionarioR GET POST
    /cad/departamento/  CadDepartamentoR GET POST
    /cad/profissao/  CadProfissaoR GET POST
    /cad/treinamento/  CadTreinamentoR GET POST
    /rel/treinamentofuncionario/ CadTreinamentoFuncR GET POST
    
    /list/departamento/ ListDepartamentoR GET
    /list/profissao/ ListProfissaoR GET
    /list/funcionario/ ListFuncionarioR GET
    /list/treinamento/ ListTreinamentoR GET
    /list/treinamentofuncionario/ ListTreinamentoFuncR GET
    
    
    /detalhe/funcionario/#FuncionariosId DetalheFuncionarioR GET POST
    
    /detalhe/departamento/#DepartamentoId DetalheDepartamentoR GET POST
    
    /detalhe/profissao/#ProfissaoId DetalheProfissaoR GET POST
    
    /detalhe/treinamento/#TreinamentoId DetalheTreinamentoR GET POST

    
    /sucesso SucessoR GET
    /erro ErroR GET
    /sucesso2 Sucesso2R GET
    /erro2 Erro2R GET
|]
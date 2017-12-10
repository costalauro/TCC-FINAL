{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Routes
import Prelude
import Yesod
import Yesod.Static
import Data.Time
import qualified Data.Text as T
import Data.Text
import Yesod.Form.Jquery
import Profile
import TipoTreinamento
import Presenca
import Confirmacao
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
import Prelude
    
data Sistreina = Sistreina {getStatic :: Static, connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Departamento
    nome Text
    sigla Text sqltype=varchar(3)
    deriving Show

Treinamento
    nome Text
    userid UsuarioId
    sala Text
    sigla Text sqltype=varchar(10)
    qtdPessoas Int
    profid ProfissaoId
    dataVal Day
    dataAplic Day
    tempoDuracao Text
    tipo TipoTreinamento
    presenca Presenca
    deriving Show

Funcionarios
   nome Text
   rg Text
   userid UsuarioId
   dataNasc Day
   deptoid DepartamentoId
   profid ProfissaoId
   deriving Show

TreinaFunc
   treinaid TreinamentoId
   funcid FuncionariosId
   deriving Show

Profissao
   nome Text
   sigla Text sqltype=varchar(10)
   deptoid DepartamentoId
   deriving Show
   
Usuario
   nome Text
   email Text
   senha Text
   tipo Profile
   UniqueEmail email
   deriving Read Show
|]

staticFiles "static"
mkMessage "Sistreina" "messages" "pt-BR"

mkYesodData "Sistreina" pRoutes
instance Yesod Sistreina where 
    
    errorHandler other = defaultErrorHandler other
    
    authRoute _ = Just LoginR
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized HomeR _      = return Authorized
    isAuthorized LoginR _     = return Authorized
    isAuthorized ErroR _      = return Authorized
    isAuthorized SucessoR _   = return Authorized
    isAuthorized Erro2R _      = return Authorized
    isAuthorized Sucesso2R _   = return Authorized
    isAuthorized FuncR _ = return Authorized
    isAuthorized RespR _ = return Authorized
    isAuthorized CadUsuarioR _  =return Authorized
    isAuthorized CadDepartamentoR _  = return Authorized
    isAuthorized CadProfissaoR _  = return Authorized
    isAuthorized CadFuncionarioR _  = return Authorized
    isAuthorized CadTreinamentoR _  = return Authorized
    isAuthorized ListDepartamentoR _  = return Authorized
    isAuthorized ListProfissaoR _  = return Authorized
    isAuthorized ListFuncionarioR _  = return Authorized
    isAuthorized ListTreinamentoR _  = return Authorized
    isAuthorized ListTodosTreinamentoR _ = return Authorized
    isAuthorized ListMeusTreinamentoR _ = return Authorized
    isAuthorized _ _          = isUser
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing      -> AuthenticationRequired
        Just "Responsavel" -> Authorized
        Just _       -> Unauthorized "Somente responsáveis tem este tipo de acesso!"
        
ehAdmin :: Handler AuthResult
ehAdmin = do 
    sess <- lookupSession "_USR"
    usr <- return $ fmap (read . unpack) sess :: Handler (Maybe Usuario)
    case usr of 
        Nothing -> return AuthenticationRequired
        Just (Usuario _ _ _ Responsavel) -> return Authorized
        Just _ -> return $ Unauthorized "VC NAO PERMISSAO"

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized
        
instance YesodPersist Sistreina where
   type YesodPersistBackend Sistreina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
       
instance RenderMessage Sistreina FormMessage where
    renderMessage _ _ = defaultFormMessage
    
type Form a = Html -> MForm Handler (FormResult a, Widget)

instance YesodJquery Sistreina where
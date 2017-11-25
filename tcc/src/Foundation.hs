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
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
    
data Sistreina = Sistreina {getStatic :: Static, connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Departamento
    nome Text
    sigla Text sqltype=varchar(3)
    deriving Show

Treinamento
    nome Text
    responsavel Text
    sala Text
    sigla Text sqltype=varchar(10)
    qtdPessoas Int
    profid ProfissaoId
    dataVal Day
    dataAplic Day
    deriving Show

Funcionarios
   nome Text
   idade Int
   salario Double
   dataNasc Day
   deptoid DepartamentoId
   profid ProfissaoId
   deriving Show
   
TreinaFunc
   treinaid TreinamentoId
   profid ProfissaoId
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
|]

staticFiles "static"
mkMessage "Sistreina" "messages" "pt-BR"

mkYesodData "Sistreina" pRoutes
instance Yesod Sistreina where 
    
    errorHandler other = defaultErrorHandler other
    
    authRoute _ = Just LoginR
    
    isAuthorized HomeR _      = return Authorized
    isAuthorized LoginR _     = return Authorized
    
    isAuthorized _ _          = isUser
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing      -> AuthenticationRequired
        Just "Responsavel" -> Authorized
        Just _       -> Unauthorized "Somente responsáveis tem este tipo de acesso!"

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
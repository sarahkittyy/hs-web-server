{-# LANGUAGE DeriveGeneric #-}

module Main where
    
import Happstack.Server
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import GHC.Generics

conf :: Conf
conf = Conf { port = 3000
            , validator = Nothing
            , logAccess = Nothing
            , threadGroup = Nothing
            , timeout = 0}

main :: IO ()
main = do
    putStrLn "Starting..."
    simpleHTTP conf $ app

app :: ServerPartT IO Response
app = msum [ mzero
           , dir "home" $ do
                             method GET
                             ok $ toResponse $ "This is the homepage!"
           , dir "api" api
           , notFound $ toResponse $ "Endpoint not found"]
           
getBody :: ServerPart L.ByteString
getBody = do
    req <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Nothing -> return $ L.pack ""
        Just rqbody -> return $ unBody $ rqbody
        
data Credentials = Credentials { username :: String, password :: String } deriving (Show, Generic)
instance A.FromJSON Credentials
instance A.ToJSON Credentials
                           
api :: ServerPartT IO Response
api = do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    msum [ mzero
         , dir "fact" $ do
                            method POST
                            val <- look "input"
                            (ok . toResponse . show) $ fact (read val)
         , dir "prime" $ do
                            method POST
                            val <- look "input"
                            (ok . toResponse . show) $ prime (read val)
         , dir "auth" $ do
                            method POST
                            body <- getBody
                            let bodyJSON = A.decode body :: Maybe Credentials
                            case bodyJSON of
                                Nothing -> badRequest $ toResponse $ "Cannot get credentials from request body."
                                Just (Credentials user pass) -> if user == "sarah" && pass == "secret"
                                                then ok $ toResponse $ "Credentials Accepted"
                                                else unauthorized $ toResponse $ "Invalid Credentials"

         , path $ \p -> notFound $ toResponse $ "API Endpoint /" ++ p ++ " not found." ]
          
fact :: Integer -> Integer
fact = product . enumFromTo 1

prime :: Integer -> Bool
prime n = all (/=0) [n `mod` m | m <- [2..pred n]]
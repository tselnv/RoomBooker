-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Booker where
import Control.Monad.IO.Class (MonadIO)


import qualified Domain.Model as M
import Configuration.Dotenv (parseFile)
import qualified Adapter.Chat as B
import Data.Either.Combinators (maybeToRight)
import qualified Data.Text as T
import Domain.Model (Booker(getFreeSpace))
import Control.Monad.Reader (ReaderT (runReaderT))


type AppState = ()


newtype BookerApp a = BookerApp {runBookerApp :: ReaderT AppState IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance M.AppChatModel BookerApp where
    processMessage = processMessageImpl

instance M.Booker BookerApp where
    -- getFreeSpace :: Room -> Date -> m Int
    getFreeSpace room date = pure 7


processMessageImpl :: M.UserId -> M.Msg -> BookerApp [M.MessageAction]
processMessageImpl uId "MenuDoNothing_callback" = pure []
processMessageImpl uId "MenuReserve_callback" = do
    freeSpace <- getFreeSpace "" (M.Date 0 0 0)
    if (freeSpace > 0)
        then pure [M.SendMessageAction uId ("Room is reserved for person: " <> T.pack (show uId))]
        else pure [M.SendMessageAction uId "Reservation denied. Room is already full"]
processMessageImpl uId _ = do
    freeSpace <- getFreeSpace "" (M.Date 0 0 0)
    if (freeSpace > 0)
        then pure [ M.ShowMenu uId ("Free space: " <> T.pack (show freeSpace)) [M.MenuReserve, M.MenuDoNothing] ]
        else pure [M.SendMessageAction uId ("Room is already full")]



runBot :: FilePath -> IO ()
runBot envFile = do
  cfg <- getCfg <$> parseFile envFile
  case cfg of
    Left err -> error err
    Right tokenStr -> 
        let recievedMessage' = \uId msg -> runReaderT (runBookerApp $ processMessageImpl uId msg) ()
        in B.botStartup tokenStr (B.handleDefaultAction recievedMessage')
  where
    getCfg :: [(String, String)] -> Either String B.TokenStr
    getCfg env = maybeToRight "No TOKEN defined" (lookup "BOT_TOKEN" env)
    

    

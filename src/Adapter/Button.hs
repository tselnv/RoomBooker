{-# LANGUAGE OverloadedStrings #-}

module Adapter.Button where



{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Telegram.Bot.API.Types
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)

type Model = ()

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  | Button1
  | Button2
  deriving (Show)

-- instance UpdateUser Model where
--   updateUser _ = id


botStartup :: String -> (Action -> Model -> Eff Action Model) -> IO ()
botStartup tokenStr handleAction = do
  let token = ""
  env <- liftIO $ defaultTelegramClientEnv token
  -- liftIO $ startBot_ (conversationBot updateChatId (incexpBotApp handleAction)) env
  liftIO $ startBot_ bot env




button1 :: InlineKeyboardButton
button1 = InlineKeyboardButton {inlineKeyboardButtonText = "Button 1", inlineKeyboardButtonCallbackData = Just "button1"}

button2 :: InlineKeyboardButton
button2 = InlineKeyboardButton {inlineKeyboardButtonText = "Button 2", inlineKeyboardButtonCallbackData = Just "button2"}



buttonHandler :: Action -> Model -> Eff Action Model
buttonHandler Button1 model = model <# (liftIO $ putStrLn "Button 1 pressed" >> pure ())
buttonHandler Button2 model = model <# (liftIO $ putStrLn "Button 2 pressed" >> pure ())

bot :: BotApp Model Action
bot = 
  BotApp
    { botInitialModel = (),
      botAction = flip handleUpdate,
      botHandler = undefined, -- handleUpdate,
      botJobs = []
    }
  where
    handleUpdate :: Model -> Update -> Maybe Action
    handleUpdate _ update = do
      msg <- updateMessage update
      usr <- messageFrom msg
      let Telegram.UserId usrId = Telegram.userId usr
      let Telegram.MessageId msgId = Telegram.messageMessageId msg
      let usrIdInt = fromIntegral usrId :: Int
      let msgIdInt = fromIntegral msgId :: Int
      let usrName = Telegram.userUsername usr
      let parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
      parseUpdate parser update

-- main :: IO ()
-- main = do
--   let token = "YOUR_BOT_TOKEN" -- Replace with your bot token
--   manager <- newManager tlsManagerSettings
--   runTelegramClient token (botApp bot) defaultTelegramClientEnv manager


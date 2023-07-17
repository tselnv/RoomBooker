{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Adapter.Chat where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Debug.Trace (traceShow)
import qualified Domain.Model as M
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import qualified Domain.Model as M
import Data.Foldable (traverse_)


type TokenStr = String

data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel
  = ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  deriving (Show, Read)


botStartup :: TokenStr -> (Action -> ChatModel -> Eff Action ChatModel) -> IO ()
botStartup tokenStr handleAction = do
  let token = Token . pack $ tokenStr
  env <- liftIO $ defaultTelegramClientEnv token
  -- liftIO $ startBot_ (conversationBot updateChatId (botApp handleAction)) env
  liftIO $ startBot_ (botApp handleAction) env



botApp :: (Action -> ChatModel -> Eff Action ChatModel) -> BotApp ChatModel Action
botApp handleAction = 
  BotApp 
    { botInitialModel = emptyChatModel, 
      botAction = flip handleUpdate, 
      botHandler = handleAction, 
      botJobs = []
    }
  where
    emptyChatModel :: ChatModel
    emptyChatModel = ChatModel InitSate


handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate _ update = do  
  let buttonCallback = updateCallbackQuery update >>= callbackQueryData  
  case buttonCallback of
    Nothing -> do
      msg <- updateMessage update
      usr <- messageFrom msg
      let Telegram.UserId usrId = Telegram.userId usr
      let Telegram.MessageId msgId = Telegram.messageMessageId msg
      let usrIdInt = fromIntegral usrId :: Int
      let msgIdInt = fromIntegral msgId :: Int
      let usrName = Telegram.userUsername usr
      let parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
      parseUpdate parser update
    Just callBack -> Just (RecordMsg 0 Nothing 0 callBack)




handleDefaultAction :: (M.UserId -> M.Msg -> IO [M.MessageAction]) -> Action -> ChatModel -> Eff Action ChatModel
handleDefaultAction processMessage act model  = 
  case act of
    NoAction -> pure model
    _ -> model <#  handleDefaultAction' act
  where
    handleDefaultAction' :: Action -> BotM Action
    handleDefaultAction' action@(RecordMsg usrId _ _ userMsg ) = traceShow action $ do
      actions :: [M.MessageAction] <- liftIO $ processMessage usrId userMsg
      traverse_ doAction actions
      pure NoAction
      where
        doAction :: M.MessageAction -> BotM Action
        doAction = \case
          M.NoMessageAction -> pure NoAction
          M.SendMessageAction uId msg -> replyString msg >> pure NoAction
          M.ShowMenu uId msg menuItems -> reply (buttonsMessage msg menuItems) >> pure NoAction
    
    handleDefaultAction' NoAction = pure NoAction
    
    replyString :: Text -> BotM ()
    replyString = reply . toReplyMessage


textShow :: Show a => a -> Text
textShow = pack . show


mkButton :: M.MenuItem -> InlineKeyboardButton
mkButton menuItem = 
   let btn = labeledInlineKeyboardButton "" 
   in btn {inlineKeyboardButtonText = textShow menuItem, inlineKeyboardButtonCallbackData = Just (textShow menuItem <> "_callback")}

buttonsMessage :: Text -> [M.MenuItem] -> ReplyMessage
buttonsMessage message menuItems = 
  let repMsg = toReplyMessage message
  in repMsg {replyMessageReplyMarkup = Just $ 
      SomeInlineKeyboardMarkup InlineKeyboardMarkup { inlineKeyboardMarkupInlineKeyboard = [ [mkButton itm] | itm <- menuItems]}}



-- button1 :: InlineKeyboardButton
-- button1 = 
--   let btn = labeledInlineKeyboardButton "" 
--   in btn {inlineKeyboardButtonText = "Button 1", inlineKeyboardButtonCallbackData = Just "button1"}

-- button2 :: InlineKeyboardButton
-- button2 = 
--   let btn = labeledInlineKeyboardButton "" 
--   in btn {inlineKeyboardButtonText = "Button 2", inlineKeyboardButtonCallbackData = Just "button2"}


-- buttonsMessage :: ReplyMessage
-- buttonsMessage = 
--   let repMsg = toReplyMessage "Choose an option:"
--   in repMsg {replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup InlineKeyboardMarkup { inlineKeyboardMarkupInlineKeyboard =[[button1], [button2]]}}


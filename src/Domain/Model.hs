module Domain.Model where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack)
import Data.Time (UTCTime)

type UserId = Int
type Msg = Text


data MenuItem = MenuReserve | MenuCancelReservation | MenuDoNothing
  deriving (Show, Eq)

data MessageAction 
  = NoMessageAction
  | ShowMenu UserId Msg [MenuItem]
  | SendMessageAction UserId Msg
  deriving (Show, Eq)



type Year = Int
type Month = Int
type Day = Int
type Room = String


data Person = Person
data Date = Date Year Month Day


-- type App m = (Booker m, Chat m)

class MonadIO m => Booker m where
    -- reserveRoom :: Room -> Date -> Person -> m (Either String String)
    getFreeSpace :: Room -> Date -> m Int
    -- reserveConfirmation :: Room -> Date -> Person -> m (Either String String)
    -- freeReservation :: Room -> Date -> Person -> m (Either String String)

-- class MonadIO m => Chat m where
--     sendMessage :: String -> Person -> m ()
--     recieveMessage :: Person -> m String
--     sendCalendar :: Person -> m ()
--     recieveDate :: Person -> m Date



class MonadIO m => AppChatModel m where
  processMessage :: UserId -> Msg -> m [MessageAction]

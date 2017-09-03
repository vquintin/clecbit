{-# LANGUAGE OverloadedStrings #-}

module ClecBit.XML
  ( getSports
  , xpSports
  , Sports(..)
  , Sport(..)
  , Event(..)
  , Match(..)
  , Bets(..)
  , Bet(..)
  , Choice(..)
  ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Ratio as R (Ratio)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Time as T
import qualified Network.HTTP.Simple as H
import qualified Text.XML.HXT.Core as HXT

getSports :: IO Sports
getSports = getRawXML >>= textToXML

textToXML :: Text -> IO Sports
textToXML t = do
  [doc] <-
    HXT.runX (HXT.readString conf (unpack t) HXT.>>> HXT.xunpickleVal xpSports)
  return doc
  where
    conf =
      [ HXT.withValidate HXT.no
      , HXT.withRemoveWS HXT.yes
      , HXT.withInputEncoding HXT.utf8
      ]

getRawXML :: IO Text
getRawXML = (toStrict . decodeUtf8 . BL.drop 3 . H.getResponseBody) <$> getResp
  where
    getResp = H.httpLBS "http://xml.cdn.betclic.com/odds_en.xml"

data Sports = Sports
  { fileDate :: T.UTCTime
  , sports :: M.Map Int Sport
  } deriving (Eq, Show)

instance HXT.XmlPickler Sports where
  xpickle = xpSports

xpSports :: HXT.PU Sports
xpSports =
  HXT.xpElem "sports" $
  HXT.xpWrap (uncurry Sports, fileDate &&& sports) $
  HXT.xpPair (HXT.xpAttr "file_date" $ xpUTCTime wet "%FT%X%Q") (xpMap "sport")

data Sport = Sport
  { sportName :: Text
  , events :: M.Map Int Event
  } deriving (Eq, Show)

instance HXT.XmlPickler Sport where
  xpickle = xpSport

xpSport :: HXT.PU Sport
xpSport =
  HXT.xpWrap (uncurry Sport, sportName &&& events) $
  HXT.xpPair (xpTextAttr "name") (xpMap "event")

data Event = Event
  { eventName :: Text
  , matches :: M.Map Int Match
  } deriving (Eq, Show)

instance HXT.XmlPickler Event where
  xpickle = xpEvent

xpEvent :: HXT.PU Event
xpEvent =
  HXT.xpWrap (uncurry Event, eventName &&& matches) $
  HXT.xpPair (xpTextAttr "name") (xpMap "match")

data Match = Match
  { startDate :: T.UTCTime
  , matchName :: Text
  , bets :: Bets
  } deriving (Eq, Show)

instance HXT.XmlPickler Match where
  xpickle = xpMatch

xpMatch :: HXT.PU Match
xpMatch =
  HXT.xpFilterAttr (HXT.hasName "name" HXT.<+> HXT.hasName "start_date") $
  HXT.xpWrap (HXT.uncurry3 Match, \t -> (startDate t, matchName t, bets t)) $
  HXT.xpTriple
    (HXT.xpAttr "start_date" $ xpUTCTime wet "%FT%X")
    (xpTextAttr "name")
    HXT.xpickle
  where
    maybeToInt m =
      if isNothing m
        then 0
        else 1

wet :: T.TimeZone
wet = T.TimeZone 60 True "WET"

xpUTCTime :: T.TimeZone -> String -> HXT.PU T.UTCTime
xpUTCTime timeZone format =
  HXT.xpWrapMaybe (stringToTime, timeToString) HXT.xpText
  where
    stringToTime s =
      T.zonedTimeToUTC <$>
      (T.ZonedTime <$> T.parseTimeM False T.defaultTimeLocale format s <*>
       Just timeZone)
    timeToString t =
      T.formatTime T.defaultTimeLocale format $ T.utcToZonedTime timeZone t

newtype Bets = Bets
  { betMap :: M.Map Int Bet
  } deriving (Eq, Show)

instance HXT.XmlPickler Bets where
  xpickle = xpBets

xpBets :: HXT.PU Bets
xpBets = HXT.xpElem "bets" $ HXT.xpWrap (Bets, betMap) (xpMap "bet")

data Bet = Bet
  { betCode :: Text
  , betName :: Text
  , choices :: M.Map Int Choice
  } deriving (Eq, Show)

instance HXT.XmlPickler Bet where
  xpickle = xpBet

xpBet :: HXT.PU Bet
xpBet =
  HXT.xpWrap (HXT.uncurry3 Bet, \t -> (betCode t, betName t, choices t)) $
  HXT.xpTriple (xpTextAttr "code") (xpTextAttr "name") (xpMap "choice")

xpMap :: (HXT.XmlPickler a) => Text -> HXT.PU (M.Map Int a)
xpMap tag = HXT.xpMap (unpack tag) "id" HXT.xpPrim HXT.xpickle

data Choice = Choice
  { choiceName :: Text
  , choiceOdd :: Double
  } deriving (Eq, Show)

instance HXT.XmlPickler Choice where
  xpickle = xpChoice

xpChoice :: HXT.PU Choice
xpChoice =
  HXT.xpWrap (uncurry Choice, choiceName &&& choiceOdd) $
  HXT.xpPair (xpTextAttr "name") (HXT.xpAttr "odd" HXT.xpPrim)

xpTextAttr :: Text -> HXT.PU Text
xpTextAttr t = HXT.xpWrap (pack, unpack) ((HXT.xpTextAttr . unpack) t)

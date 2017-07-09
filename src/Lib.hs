module Lib
    ( someFunc
    , xpSports
    , XMLSports (..)
    , XMLSport (..)
    , XMLEvent (..)
    ) where
import Control.Arrow ((&&&))
import qualified Data.Ratio as R (Ratio)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Text.XML.HXT.Core as HXT

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data XMLSports = XMLSports
  { fileDate :: T.UTCTime
  , sports :: M.Map Int XMLSport
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLSports where
  xpickle = xpSports

xpSports :: HXT.PU XMLSports
xpSports =
  HXT.xpElem "sports" $
  HXT.xpWrap (uncurry XMLSports, fileDate &&& sports) $
  HXT.xpPair (HXT.xpAttr "file_date" $ xpUTCTime wet "%FT%X%Q")
             (xpMap "sport")

data XMLSport = XMLSport
  { sportName :: String
  , events :: M.Map Int XMLEvent
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLSport where
  xpickle = xpSport

xpSport :: HXT.PU XMLSport
xpSport =
  HXT.xpWrap (uncurry XMLSport, sportName &&& events) $
  HXT.xpPair (HXT.xpTextAttr "name")
             (xpMap "event")

data XMLEvent = XMLEvent
  { eventName :: String
  , matches :: M.Map Int XMLMatch
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLEvent where
  xpickle = xpEvent

xpEvent :: HXT.PU XMLEvent
xpEvent =
  HXT.xpWrap (uncurry XMLEvent, eventName &&& matches) $
  HXT.xpPair (HXT.xpTextAttr "name")
             (xpMap "match")

data XMLMatch = XMLMatch
  { startDate :: T.UTCTime
  , matchName :: String
  , bets :: XMLBets
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLMatch where
  xpickle = xpMatch

xpMatch :: HXT.PU XMLMatch
xpMatch =
  HXT.xpWrap ( HXT.uncurry3 XMLMatch
             , \t -> (startDate t, matchName t, bets t)
             ) $
  HXT.xpTriple (HXT.xpAttr "start_date" $ xpUTCTime wet "%FT%X")
               (HXT.xpTextAttr "name")
               HXT.xpickle

wet :: T.TimeZone
wet = T.TimeZone 60 True "WET"

xpUTCTime :: T.TimeZone -> String -> HXT.PU T.UTCTime
xpUTCTime timeZone format =
  HXT.xpWrapMaybe ( stringToTime
                  , timeToString
                  ) HXT.xpText
  where
    stringToTime s = T.zonedTimeToUTC <$> (T.ZonedTime <$> T.parseTimeM False T.defaultTimeLocale format s <*> Just timeZone)
    timeToString t = T.formatTime T.defaultTimeLocale format $ T.utcToZonedTime timeZone t

newtype XMLBets = XMLBets
  { betMap :: M.Map Int XMLBet
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLBets where
  xpickle = xpBets

xpBets :: HXT.PU XMLBets
xpBets =
  HXT.xpElem "bets" $
  HXT.xpWrap (XMLBets, betMap) (xpMap "bet")

data XMLBet = XMLBet
  { betCode :: String
  , betName :: String
  , choices :: M.Map Int XMLChoice
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLBet where
  xpickle = xpBet

xpBet :: HXT.PU XMLBet
xpBet =
  HXT.xpElem "bet" $
  HXT.xpWrap ( HXT.uncurry3 XMLBet
             , \t -> (betCode t, betName t, choices t)
             ) $
  HXT.xpTriple (HXT.xpAttr "code" HXT.xpText)
               (HXT.xpAttr "name" HXT.xpText)
               (xpMap "choice")


xpMap :: (HXT.XmlPickler a) => String -> HXT.PU (M.Map Int a)
xpMap tag = HXT.xpMap tag "id" HXT.xpPrim HXT.xpickle

data XMLChoice = XMLChoice
  { choiceName :: String
  , choiceOdd :: Double
  } deriving (Eq, Show)

instance HXT.XmlPickler XMLChoice where
  xpickle = xpChoice

xpChoice :: HXT.PU XMLChoice
xpChoice =
  HXT.xpElem "choice" $
  HXT.xpWrap ( uncurry XMLChoice, choiceName &&& choiceOdd) $
  HXT.xpPair (HXT.xpAttr "name" HXT.xpText)
             (HXT.xpAttr "odd"  HXT.xpPrim)

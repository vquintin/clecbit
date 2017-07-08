module Lib
    ( someFunc
    ) where
import qualified Data.Dates as D (DateTime)
import qualified Data.Ratio as R (Ratio)
import qualified Data.Map as M
import qualified Text.XML.HXT.Core as HXT

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data XMLSports = XMLSports
  { fileDate :: D.DateTime
  , sports :: M.Map Int XMLSport
  }

data XMLSport = XMLSport
  { sportName :: String
  , matches :: M.Map Int XMLMatch
  }

data XMLMatch = XMLMatch
  { startDate :: D.DateTime
  , matchName :: String
  , bets :: M.Map Int XMLBet
  }

data XMLBet = XMLBet
  { betCode :: String
  , betName :: String
  , choices :: M.Map Int XMLChoice
  }

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
xpMap tag =
  HXT.xpWrap (M.fromList, M.toList) $
  HXT.xpList $
  HXT.xpElem tag $
  HXT.xpPair ( HXT.xpAttr "id" HXT.xpPrim) HXT.xpickle

data XMLChoice = XMLChoice
  { choiceName :: String
  , choiceOdd :: R.Ratio Int
  }

instance HXT.XmlPickler XMLChoice where
  xpickle = xpChoice

xpChoice :: HXT.PU XMLChoice
xpChoice =
  HXT.xpElem "choice" $
  HXT.xpWrap ( uncurry XMLChoice
         , \t -> (choiceName t, choiceOdd t)
         ) $
  HXT.xpPair (HXT.xpAttr "name" HXT.xpText)
             (HXT.xpAttr "odd"  HXT.xpPrim)

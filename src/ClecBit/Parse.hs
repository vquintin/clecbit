{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ClecBit.Parse
  ( parseSports
  ) where

import ClecBit.Parse.Football
import ClecBit.Parse.Parsable
import ClecBit.XML
import qualified Data.Map as M
import Data.Text (Text)
import qualified HBet.Bet as HB
import qualified HBet.BetSum as HBS
import qualified HBet.Types as TY

parseSports :: Sports -> [Either String (HBS.ChoiceSum ())]
parseSports sps = do
  (_, sport) <- M.toList $ sports sps
  (_, event) <- M.toList $ events sport
  case sportName sport of
    "Football" -> fmap HBS.Football <$> parseEvent event
    s -> (return . Left) $ "Unknown sport: " ++ show s

parseEvent ::
     ( Parsable Text (HB.Competition sport)
     , Parsable Text (HB.Lineup sport)
     , Parsable (Text, Text) (HB.BetType sport)
     )
  => Event
  -> [Either String (HB.Choice sport ())]
parseEvent ev = do
  let competition = parseData $ eventName ev
  (_, match) <- M.toList $ matches ev
  let lineup = parseData $ matchName match
  (_, bet) <- M.toList $ (betMap . bets) match
  (_, choice) <- M.toList $ choices bet
  let betType = parseData (betCode bet, choiceName choice)
  return $
    HB.Choice <$> pure () <*> (HB.Match <$> competition <*> lineup) <*> betType <*>
    pure (choiceOdd choice)

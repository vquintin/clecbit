{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ClecBit.Parse.Football
  (
  ) where

import ClecBit.Parse.Parsable
import qualified Data.Map as M
import Data.Text (Text)
import qualified HBet.Bet as HB
import qualified HBet.Football as FB
import qualified HBet.Type as TY

parseFootball :: Event -> [Either String (HB.Choice FB.Football ())]
parseFootball ev = do
  let competition = parseData $ eventName ev
  (_, match) <- M.toList $ matches ev
  let lineup = parseData $ matchName match
  (_, bet) <- M.toList $ (betMap . bets) match
  (_, choice) <- M.toList $ choices bet
  let betType = parseData (betCode bet, choiceName choice)
  return $
    HB.Choice <$> pure () <*> (HB.Match <$> competition <*> lineup) <*> betType <*>
    pure (choiceOdd choice)

instance Parsable Text (HB.Competition FB.Football) where
  parseData t =
    case t of
      "Eng. Premier League" -> Right $ FB.ChD1 TY.England
      "French Ligue 1" -> Right $ FB.ChD1 TY.France
      "German Bundesliga" -> Right $ FB.ChD1 TY.Germany
      "Italian Serie A" -> Right $ FB.ChD1 TY.Italy
      "Spanish Liga Primera" -> Right $ FB.ChD1 TY.Spain
      "French Ligue 2" -> Right $ FB.ChD2 TY.France
      "Belgian First Division A" -> Right $ FB.ChD1 TY.Belgium
      "English Championship" -> Right $ FB.ChD2 TY.England
      "German Bundesliga 2" -> Right $ FB.ChD2 TY.Germany
      "Italian Serie B" -> Right $ FB.ChD2 TY.Italy
      "Brazilian Serie A" -> Right $ FB.ChD1 TY.Brasil
      _ -> Left $ "Unknown competition: " ++ show t

instance Parsable Text (HB.Lineup FB.Football) where
  parseData t =
    case splitOn " - " t of
      [a, b] -> return $ Lineup a b
      _ -> Left $ "Can't parse lineup: " ++ show t

instance Parsable (Text, Text) (HB.BetType FB.Football) where
  parseData (bet, choice) =
    case bet of
      "Ftb_Mr3" -> FB.FullTimeWinOrDraw <$> parseData choice
      "Ftb_Dbc" ->
        case splitOn " or " choice of
          [a, b] -> FB.DoubleChance <$> parseData a <*> parseData b
          _ -> Left $ "Can't parse double chance: " ++ show choice
      "Ftb_Csc" ->
        case splitOn " - " choice of
          [a, b] -> FB.ExactScore <$> parseData a <*> parseData b
          _ -> Left $ "Can't parse exact score: " ++ show choice
      "Ftb_Htr" -> FB.HalfTimeWinOrDraw <$> parseData choice
      "Ftb_Htf" ->
        case splitOn " / " choice of
          [a, b] -> FB.HTFTWinOrDraw <$> parseData a <*> parseData b
          _ -> Left $ "Can't parse HT/FT: " ++ show choice

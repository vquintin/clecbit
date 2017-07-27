module ClecBit.Bet
  ( BetSum(..)
  , toBetSum
  ) where

import ClecBit.XML
import qualified Control.Monad as MO
import qualified Data.List.Split as SP
import qualified Data.Map as M
import qualified HBet.Bet as HB
import qualified HBet.Football as FB

data BetSum
  = FootballFullTime (FB.FootballBetInfo FB.FootballFullTime)
  | FootballHalfTime (FB.FootballBetInfo FB.FootballHalfTime)
  | FootballHalfFullTime (FB.FootballBetInfo FB.FootballHalfFullTime)
  deriving (Show)

toBetSum :: Sports -> [BetSum]
toBetSum s = MO.msum sums
  where
    sums = (sportToBetSum . snd) <$> (M.toList . sports) s

sportToBetSum :: Sport -> [BetSum]
sportToBetSum s =
  case sportName s of
    "Football" -> footBallToBetSum s
    _ -> MO.mzero

footBallToBetSum :: Sport -> [BetSum]
footBallToBetSum sport = do
  (eventID, event) <- (M.toList . events) sport
  (matchID, match) <- (M.toList . matches) event
  (betID, bet) <- (M.toList . betMap . bets) match
  hEvent <- (getEvent . eventName) event
  (team1, team2) <- (getTeams . matchName) match
  let hMatch = FB.FootballMatch team1 team2 hEvent
  footballBetToBetSum hMatch bet
  where
    getEvent "World Cup" = [FB.WorldCup]
    getEvent "Eng. Premier League" = [FB.PremierLeague]
    getEvent "French Ligue 1" = [FB.Ligue1]
    getEvent "Spanish Liga Primera" = [FB.LigaPrimera]
    getEvent _ = []
    getTeams s =
      case SP.splitOn " - " s of
        [t1, t2] -> [(t1, t2)]
        _ -> []

footballBetToBetSum :: FB.FootballMatch -> Bet -> [BetSum]
footballBetToBetSum match bet =
  case betCode bet of
    "Ftb_Mr3" ->
      return $
      FootballFullTime $
      FB.FootballBetInfo match (betToHbet strToFootballFullTime bet)
    "Ftb_Htr" ->
      return $
      FootballHalfTime $
      FB.FootballBetInfo match (betToHbet strToFootballHalfTime bet)
    "Ftb_Htf" ->
      return $
      FootballHalfFullTime $
      FB.FootballBetInfo match (betToHbet strToFootballHalfFullTime bet)
    _ -> []

betToHbet :: (String -> [a]) -> Bet -> [HB.Choice a]
betToHbet fa bet = do
  (choiceID, choice) <- (M.toList . choices) bet
  betType <- fa $ choiceName choice
  let odd = choiceOdd choice
  return $ HB.Choice betType odd

strToFootballFullTime :: String -> [FB.FootballFullTime]
strToFootballFullTime "%1%" = return FB.FT1
strToFootballFullTime "Draw" = return FB.FTDraw
strToFootballFullTime "%2%" = return FB.FT2
strToFootballFullTime _ = MO.mzero

strToFootballHalfTime :: String -> [FB.FootballHalfTime]
strToFootballHalfTime "%1%" = return FB.HT1
strToFootballHalfTime "Draw" = return FB.HTDraw
strToFootballHalfTime "%2%" = return FB.HT2
strToFootballHalfTime _ = MO.mzero

strToFootballHalfFullTime :: String -> [FB.FootballHalfFullTime]
strToFootballHalfFullTime s = do
  (htStr, ftStr) <- splitResult s
  ht <- strToFootballHalfTime htStr
  ft <- strToFootballFullTime ftStr
  return $ FB.FootballHalfFullTime (ht, ft)
  where
    splitResult s =
      case SP.splitOn " / " s of
        [a, b] -> [(a, b)]
        _ -> []

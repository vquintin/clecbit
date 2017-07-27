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
      FootballFullTime $ FB.FootballBetInfo match (betToFootballFullTime bet)
    _ -> []

betToFootballFullTime :: Bet -> [HB.Choice FB.FootballFullTime]
betToFootballFullTime bet = (snd <$> (M.toList . choices) bet) >>= go
  where
    go (Choice "%1%" odd) = [HB.Choice FB.FT1 odd]
    go (Choice "Draw" odd) = [HB.Choice FB.FT1 odd]
    go (Choice "%2%" odd) = [HB.Choice FB.FT1 odd]
    go _ = []
